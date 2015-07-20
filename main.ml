open Lwt
open Cohttp_lwt_unix

let body_of_string s =
  `String s

let pack_string s =
  (Cohttp.Transfer.Fixed (Int64.of_int (String.length s)), body_of_string s)

let main port =
  let callback conn req body = 
    let uri = Cohttp.Request.uri req in
    let (encoding, body) =
      match Uri.path uri with
      | "/test" ->
        pack_string "warning! experimental testing procedures in place."
      | _ ->
        pack_string "hello, world!" in
    let mime_type = "text/html" in
    let headers = Cohttp.Header.add_opt None "content-type" mime_type in
    let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
    return (res, body) in
  let server = Server.make callback () in
  Server.create ~mode:(`TCP (`Port port)) server

let _ =
  let port = ref 8080 in
  let anon_fun s =
    print_endline s in
  let speclist = [
    ("-port", Arg.Set_int port, " The port to serve on. Defaults to 8080.")
  ] in
  let usage_msg =
    "An example HTTP server using cohttp and Lwt." in
  Arg.parse speclist anon_fun usage_msg ;
  Lwt_main.run (main (!port))

