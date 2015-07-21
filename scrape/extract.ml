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

let parse_table x =
  let route =
    match extract_tag "h5" x with
    | [`Data y] -> y
    | _ -> failwith "Unexpected h5 element children (route name)."
  in
  match extract_tags "tr" x with
  | _ :: stop_els :: _ :: [] :: rest_els ->
    let stops =
      List.fold_left
        (fun a l ->
           match l with
           | `El (_, [`Data x]) -> x :: a
           | _ -> a)
        [] stop_els
    in
    let times =
      List.fold_left
        (fun a l ->
           (List.fold_left
             (fun a' l' ->
                match l' with
                | `El (_, [`Data x]) -> x :: a'
                | _ -> a')
             [] l) :: a)
        [] rest_els
    in
    {route; stops; times}
  | _ -> failwith "Failed to find expected trs (stop names and stops)."

let validate {route = _; stops; times} =
  let nstops = List.length stops in
  List.iter
    (fun l ->
       if List.length l <> nstops then
         failwith "Number of times doesn't match number of stops."
       else
         ())
    times

let display data =
  let display' {route; stops; times} =
    print_endline route ;
    print_endline "MTWRFSS" ;
    print_endline "\n--\n" ;
    times |>
    List.iter
      (fun ts ->
         List.iter2
           (fun stop t ->
              if t = "--" then
                print_endline stop
              else
                Printf.printf "%s\t\t\t%s\n" stop t)
           stops ts ;
         print_endline "--") ;
    print_endline "EOF"
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

