open Lwt

(* time offset in seconds from the beginning of the day *)
type daytime = int

module Stop = struct
  type name = string

  type t = {
    name : name;
    time : daytime option
  }
end

module Trail = struct
  type t = Stop.t list
end

type name = string
(* M; W; T; R; F; S; S *)
type days = int list

type t = {
  name : string;
  days : days;
  trails : Trail.t list
}

let tsv_rex = Pcre.regexp "\t+"
let colon_rex = Pcre.regexp ":"
let separator = "--"

exception Parse_failure of string

let daytime_of_hm h m =
  3600 * h + 60 * m

let hm_of_daytime dt =
  let h = dt / 3600 in
  let m = dt mod 3600 / 60 in
  (h, m)

let parse_stop s =
  let skip_if_empty l a = if String.length l = 0 then a else l :: a in
  match List.fold_right skip_if_empty (Pcre.split ~rex:tsv_rex s) [] with
  | [name] -> {Stop.name; time = None}
  | [name; stime] ->
    let dt =
      try match Pcre.split ~rex:colon_rex stime with
        | [h; m] -> daytime_of_hm (int_of_string h) (int_of_string m)
        | [h] -> daytime_of_hm (int_of_string h) 0
        | _ -> raise Not_found
      with Failure "int_of_string" -> raise Not_found
  in {Stop.name; time=Some dt}
  | _ -> raise Not_found

let unfold_right p f g s =
  let rec unfold_right' p f g s a =
    if p s then a
    else
      unfold_right' p f g (g s) (f s :: a)
  in unfold_right' p f g s []

let thru n =
  unfold_right (fun x -> x > n) (fun x -> x) ((+) 1) 0

let parse_days days' =
  let is_day day = days'.[day] <> '_' in
  if String.length days' <> 7 then
    raise (Parse_failure "Failed to parse route days.")
  else
    List.fold_left
      (fun a l -> if is_day l then l :: a else a)
      []
      (thru 6)

let parse_file file_name =
  let st = Lwt_io.lines_of_file file_name in
  Lwt_stream.get st >>= function
  | None -> raise (Parse_failure "Failed to read route name.")
  | Some name ->
  Lwt_stream.get st >>= function
  | None -> raise (Parse_failure "Failed to read route days.")
  | Some days' ->
  let days = parse_days days' in
  let read st =
    let rec read' trails trail =
      Lwt_stream.get st >>= function
      | None ->
        return (
          if trail <> [] then trail :: trails
          else trails)
      | Some line ->
        let trimmed = String.trim line in
        if String.length trimmed = 0 || line.[0] = '#' then
          read' trails trail
        else if trimmed = separator then
          if trail <> [] then
            read' (List.rev trail :: trails) []
          else
            read' trails []
        else
          let stop =
            try parse_stop line with
            | Not_found -> raise (Parse_failure (Printf.sprintf "Expected stop, got: %s" line))
          in read' trails (stop :: trail)
    in read' [] []
  in
  read st >>= fun trails ->
  return {name; days; trails}
