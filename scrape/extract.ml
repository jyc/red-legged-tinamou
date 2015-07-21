let extract_tag tag ns =
  let n = ref None in
  ns
  |> Ezxmlm.filter_iter ~tag
    ~f:(fun _ c ->
        match (!n) with
        | None -> n := Some c
        | Some _ -> ()) ;
  match !n with
  | Some x -> x
  | None -> raise Not_found 

let extract_tags tag ns =
  let ns' = ref [] in
  ns
  |> Ezxmlm.filter_iter ~tag
    ~f:(fun attr c ->
        ns' := c :: !ns') ;
  List.rev (!ns')

type raw_data = {
  route : string;
  stops : string list;
  times : string list list
}

let fold_data els =
  els
  |> List.fold_left
    (fun a l ->
       match l with
       | `El (_, [`Data x]) -> x :: a
       | _ -> a)
    []
  |> List.rev

let ampm_rex = Pcre.regexp "([0-9]+):([0-9]+)\\s*(AM|PM)"

let mil_of_ampm s = 
  try let m = Pcre.exec ~rex:ampm_rex s in
  match Pcre.get_substrings m ~full_match:false with
  | [| hs; ms; ps |] -> 
    let h = int_of_string hs in
    let m = int_of_string ms in
    let hoff =
      (* TCAT seems to use 12:00 PM for noon *)
      (if h = 12 then -12 else 0) +
      match ps with
      | "AM" -> 0
      | "PM" -> 12
      | _ -> failwith (Printf.sprintf "Unrecognized period: %s" ps)
    in Printf.sprintf "%02d:%02d" (h + hoff) m
  | _ -> failwith ("Failed to parse time: " ^ s)
  with Not_found -> s

let parse_table x =
  let route =
    match extract_tag "h5" x with
    | [`Data y] -> y
    | _ -> failwith "Unexpected h5 element children (route name)."
  in
  match extract_tags "tr" x with
  | _ :: stop_els :: _ :: [] :: rest_els ->
    let stops = fold_data stop_els in
    let times =
      rest_els
      |> List.fold_left
        (fun a l -> (fold_data l |> List.map mil_of_ampm) :: a)
        []
      |> List.rev
    in {route; stops; times}
  | _ -> failwith "Failed to find expected trs (stop names and stops)."

let validate {route = _; stops; times} =
  let nstops = List.length stops in
  times
  |> List.iter
    (fun l ->
       if List.length l <> nstops then
         failwith "Number of times doesn't match number of stops."
       else ())

let display data =
  let display' {route; stops; times} =
    print_endline route ;
    print_endline "MTWRFSS" ;
    print_endline "\n--\n" ;
    times
    |> List.iter
      (fun ts ->
         List.iter2
           (fun stop t ->
              (* -- seems to be a placeholder for when the bus doesn't stop
                 there but they want to keep the spot in the table; F means
                 "Flag Stop. Persons wanting to board will have to signal the
                 bus driver." *)
              match t with
              | "--" -> ()
              | "F" -> print_endline stop
              | _ -> Printf.printf "%s\t\t\t%s\n" stop t)
           stops ts ;
         print_endline "--") ;
    print_endline "\nEOF"
  in List.iter display' data

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.fprintf stderr "Usage: %s <xml>\n" Sys.argv.(0) ;
    exit 1
  end else begin
    let (_, xml) = Ezxmlm.from_channel (open_in Sys.argv.(1)) in
    let data =
      extract_tags "table" xml
      |> List.map parse_table
    in
    List.iter validate data ;
    display data
  end

