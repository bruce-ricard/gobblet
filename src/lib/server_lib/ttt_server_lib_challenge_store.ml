type challenge = Ttt_server_lib_challenge.t

type t = {
    event_listener: unit React.event;
    trigger_event: unit -> unit;
    mutex: Lwt_mutex.t;
    mutable challenges: challenge list;
  }

let list_to_string elt_to_string l =
  let rec aux = function
      [x] -> elt_to_string x
    | x :: xs -> elt_to_string x ^ "; " ^ (aux xs)
    | [] -> assert false
  in
  "[" ^
    (match l with
       [] -> ""
     | _ -> aux l)
    ^ " ]"

let challenges_to_string challenges =
  list_to_string (fun i -> string_of_int (i#get_id))
  @@ List.map Ttt_server_lib_challenge.id challenges

let log_debug_challenges challenges =
  Logs.debug (fun m ->
      m "challenges: %s"
        (challenges_to_string challenges)
    )

let load () =
  let event_listener, trigger_event = React.E.create ()
  in
  {
    event_listener;
    trigger_event;
    mutex = Lwt_mutex.create ();
    challenges = [];
  }

let event_listener t = t.event_listener

let create db challenger ?opponent game_name id =
  let challenge =
    Ttt_server_lib_challenge.create ?game_name challenger ?opponent id in
  log_debug_challenges db.challenges;
  db.challenges <-
    db.challenges @ [challenge];
  log_debug_challenges db.challenges;
  (*db.trigger_event ();*)
  challenge

let public_challenges_for_user db user =
  let filter_function challenge =
    match Ttt_server_lib_challenge.opponent challenge with
    | None ->
       let challenger = Ttt_server_lib_challenge.challenger challenge in
       user <> challenger
    | _ -> false
  in

  let publics = List.filter filter_function
                            db.challenges
  in
  publics

let private_challenges_for_user db user =
  let filter_function challenge =
    match Ttt_server_lib_challenge.opponent challenge with
      Some(opp) -> user = opp
    | _ -> false
  in
  let privates = List.filter filter_function
                             db.challenges
  in
  privates

let rec remove_id id =
  let open Ttt_server_lib_types in
  function
    [] -> [], Id_not_present
  | challenge :: xs ->
     let id2 = (Ttt_server_lib_challenge.id challenge)#get_id in
     if id#get_id = id2 then
       xs, Deleted(challenge)
     else
       let a,b = remove_id id xs in
       challenge :: a, b

let remove db id =
  (* TODO: there will be race conditions here, fix *)
  log_debug_challenges db.challenges;
  let open Ttt_server_lib_types in
  let list,result = remove_id id db.challenges in
  match result with
  | Deleted(user) as d ->
     begin
       db.challenges <- list;
       log_debug_challenges db.challenges;
       d
     end
  |  Id_not_present -> Id_not_present

let purge_user_challenges db user =
  let open Ttt_server_lib_challenge in
  let rec aux = function
    | [] -> []
    | c :: cs ->
       if challenger c = user then
         begin
           Logs.debug (fun m ->
               m "Deleted challenge %d for user %s"
                 (id c)#get_id
                 user
             );
           aux cs
         end
       else
         c :: aux cs
  in
  db.challenges <- aux db.challenges

let lock db : <unlock : unit > Lwt.t =
  let open Lwt in
  Lwt_mutex.lock db.mutex >>=
    (fun lock ->
      Lwt.return
        (object
           method unlock = Lwt_mutex.unlock db.mutex
         end)
    )

let send_updates db =
  let trigger = db.trigger_event in
  let rec aux () =
    let open Lwt in
    Lwt.async (fun () ->
        Lwt_unix.sleep 3. >>=
          (fun () ->
            trigger ();
            Lwt.return (aux())
          )
      )
  in
  aux ()
