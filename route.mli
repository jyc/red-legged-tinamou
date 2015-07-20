(* time offset in seconds from the beginning of the day *)
type daytime = int

module Stop : sig
  type name = string

  type t = {
    name : name;
    time : daytime option
  }
end

module Trail : sig
  type t = Stop.t list
end

type name = string
type days = int list

type t = {
  name : name;
  days : days;
  trails : Trail.t list
}

exception Parse_failure of string

val daytime_of_hm : int -> int -> daytime
val hm_of_daytime : daytime -> int * int

val parse_file : string -> t Lwt.t
