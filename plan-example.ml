open Lwt

let files = [
  "route-10-week.route";
  "route-15-sat.route";
  "route-15-sun.route";
  "route-15-week.route";
]
;;

Lwt_list.map_p Route.parse_file (List.map (( ^ ) "routes/") files) >>= fun routes ->
let input = Planner.Input.of_routes routes in
let source = {Planner.Routestop.
               route="Route 10 Weekdays"; stop="Goldwin Smith Hall"; time=27420} in
let dest = "Wegmans" in
return (Planner.plan input source dest)
;;

(* Run utop and paste the contents of this file.
   You should see:

  - : (Planner.Routestop.t list * int) option =
  Some
   ([{Planner.Routestop.route = "Route 10"; stop = "Goldwin Smith Hall"; time = 27420};
     {Planner.Routestop.route = "Route 10"; stop = "Sage Hall"; time = 27540};
     {Planner.Routestop.route = "Route 10"; stop = "Anabel Taylor Hall"; time = 27660};
     {Planner.Routestop.route = "Route 10"; stop = "Schwartz CPA"; time = 27780};
     {Planner.Routestop.route = "Route 10"; stop = "College @ Mitchel"; time = 27900};
     {Planner.Routestop.route = "Route 10"; stop = "Seneca @ Commons"; time = 113400};
     {Planner.Routestop.route = "Route 15"; stop = "Seneca @ Commons"; time = 116460};
     {Planner.Routestop.route = "Route 15"; stop = "Titus Towers I"; time = 116880};
     {Planner.Routestop.route = "Route 15"; stop = "Wegmans"; time = 117150}],
    89730)

   Unfortunately our bus rider wasn't able to make it to the Seneca @ Commons
   bus stop on Route 15 in time, and had to wait until the next day for the
   next Route 10 to Wegmans. Her total trip time is 24.925 hours. *)
