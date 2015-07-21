type weektime = int
let week_sec = 86400 * 7

let assert_some opt =
  match opt with
  | Some x -> x
  | None -> failwith "assert_some: Given [opt] was not Some."

module Routestop = struct

  type t = {
    route : Route.name;
    stop : Route.Stop.name;
    time : weektime
  }

  let make route {Route.Stop.name; time} =
    {route; stop=name; time=assert_some time}

  let of_trail route trail' = 
    (* bs' should be in reverse order, c is not included
       n should be the number of elements in bs' + 1 *)
    let adjust_times a bs' c n =
      let at = assert_some a.Route.Stop.time in
      let ct = assert_some c.Route.Stop.time in 
      let d = float (ct - at) /. (float n) in
      let bs = fst (List.fold_left 
                      (fun (bs, i) b' ->
                         ({route = route; stop = b'.Route.Stop.name;
                           time = at + int_of_float (d *. i)} :: bs, i -. 1.0))
                      ([], float (n - 1))
                      bs')
      in (make route a) :: bs
    in
    (* rest is in order, tbd is reversed, det is reversed, return value in order *)
    let rec of_trail' anchor rest tbd n det =
      match rest with
      | ({Route.Stop.name = _; time = Some _} as x) :: xs ->
        of_trail' x xs [] 1 (List.rev_append (adjust_times anchor tbd x n) det)
      | x :: xs ->
        of_trail' anchor xs (x :: tbd) (n + 1) det
      | [] ->
        (match tbd with
         | [] ->
           (match anchor with
            | {Route.Stop.name; time = Some time} ->
              List.rev ({route; stop = name; time} :: det)
            | _ -> failwith "Assert failure: anchor time is None")
         | _ -> failwith "of_trail: The given [trail] could not be determined.")
    in
    let rec circ rest front =
      match rest with
      | {Route.Stop.name = _; time = Some _} :: xs ->
        List.rev_append front rest
      | x :: xs ->
        circ xs (x :: front)
      | [] ->
        failwith "of_trail: The given [trail] must have at least two specified times."
    in
    let trail = circ trail' [] in
    match trail with
    | a :: (b :: _ as xs) -> of_trail' a xs [] 1 []
    | _ -> failwith "routestops_of_trail: The given [trail] must have at least two times."

end

module Stopmap = struct

  (* stop name -> routestops *)
  type t = (Route.Stop.name, Routestop.t list) Hashtbl.t

  let create = Hashtbl.create 1

  let update_with map rst =
    List.iter
      (fun rs ->
         let stop = rs.Routestop.stop in
         let old = if Hashtbl.mem map stop then Hashtbl.find map stop else [] in
         Hashtbl.replace map stop (rs :: old))
      rst

  let at map name = 
    try Hashtbl.find map name with
    | Not_found -> []

end

module Nextmap = struct

  type t = (Routestop.t, Routestop.t) Hashtbl.t

  let create = Hashtbl.create 1

  let update_with map rst =
    let rec update_with' = function
      | a :: (b :: _ as rest) -> 
        Hashtbl.replace map a b ;
        update_with' rest
      | [] -> failwith "Should not be here."
      | a :: rest -> ()
    in update_with' rst

  let next map rs =
    try Some (Hashtbl.find map rs) with
    | Not_found -> None

end

(* 
   Lower priorities are at the top.
   Adapted from http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html .
*)
module PQueue = struct

  type priority = int

  type 'a t =
    | Empty
    | Node of priority * 'a * 'a t * 'a t

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
    | Empty -> Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
      if prio <= p then
        Node (prio, elt, insert right p e, left)
      else
        Node (p, e, insert right prio elt, left)

  let rec remove_top = function
    | Empty -> invalid_arg "remove_top': Can't remove from empty queue."
    | Node (prio, elt, left, Empty) -> left
    | Node (prio, elt, Empty, right) -> right
    | Node (prio, elt,
            (Node (lprio, lelt, _, _) as left),
            (Node (rprio, relt, _, _) as right)) ->
      if lprio <= rprio then
        Node (lprio, lelt, remove_top left, right)
      else
        Node (rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> None
    | Node (prio, elt, _, _) as queue -> Some (prio, elt, remove_top queue)

end

module Input = struct

  type t = {
    stops : Stopmap.t;
    nexts : Nextmap.t
  }

  let of_routes routes =
    let stops = Stopmap.create in
    let nexts = Nextmap.create in
    List.iter
      (fun {Route.name = route; days; trails} ->
         trails
         |> List.iter
           (fun trail ->
              let rst =
                let rst' = Routestop.of_trail route trail in
                days
                |> List.map
                  (fun d ->
                     rst'
                     |> List.map
                       (fun rs -> {rs with Routestop.time = d * 86400 + rs.Routestop.time}))
                |> List.flatten
              in
              Stopmap.update_with stops rst ;
              Nextmap.update_with nexts rst))
      routes ;
    {stops; nexts}

end

let transit a b =
  let at = a.Routestop.time in
  let bt = b.Routestop.time in
  (bt - at + week_sec) mod week_sec

let plan ?(stop_switch_cost=60) {Input.stops; nexts} source dest =
  let visited_ht = Hashtbl.create 1 in
  let visited = Hashtbl.mem visited_ht in
  let mark_visited n = Hashtbl.replace visited_ht n () in
  let paths = Hashtbl.create 1 in
  let frontier = ref PQueue.empty in
  let consider a (path, t') (b, transit) =
    if visited b then () else begin
      let t = t' + (transit a b) in
      let inf_dist = not (Hashtbl.mem paths b) in
      if inf_dist || (let _, t' = Hashtbl.find paths b in t' > t) then begin
        Hashtbl.replace paths b (path, t) ;
        frontier := PQueue.insert (!frontier) t b
      end else ()
    end
  in
  let rec find_dest = function
    | x :: xs ->
      if x.Routestop.stop = dest then Some x
      else find_dest xs
    | [] -> None
  in
  let rec visit_next () =
    match PQueue.extract (!frontier) with
    | Some (t, n, frontier') ->
      frontier := frontier' ;
      visit n
    | None -> None
  and visit n =
    (* This can happen if a neighbor is both a next and at the same stop.
       We try to minimize it but this seems to me the cleanest way to deal with
       that possibility so far. *)
    if visited n then visit_next ()
    else begin
      mark_visited n ;
      let stop_neighbors = Stopmap.at stops n.Routestop.stop in
      let next_neighbors =
        match Nextmap.next nexts n with
        | Some x -> [x]
        | None -> []
      in
      let neighbors = next_neighbors @ stop_neighbors in
      let neighbors_t = (next_neighbors
                         |> List.map (fun x -> (x, transit))) @
                        (stop_neighbors
                         |> List.map (fun x -> (x, (fun a b -> transit a b + stop_switch_cost))))
      in
      match find_dest neighbors with
      | Some x ->
        let path', t' = Hashtbl.find paths n in 
        Some (List.rev (x :: n :: path'), t' + (transit n x))
      | None ->
        let path, t' = Hashtbl.find paths n in
        List.iter (consider n (n :: path, t')) neighbors_t ;
        if (!frontier) = PQueue.empty then None
        else visit_next ()
    end
  in
  Hashtbl.replace paths source ([], 0) ;
  visit source
