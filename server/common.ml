open Ttt_common_lib_types

module GameRDB : Ttt_server_lib_types.REACT_DB = functor (T : sig type t end) ->
  struct
    let objects = ref []

    let put id t =
      Logs.debug (fun m -> m "Inserted game %d into RDB" id#get_id);
      objects := (id#get_id,t) :: !objects

    let delete id = objects := List.filter (fun (x,_) -> x <> id#get_id)
                                           !objects

    let get id =
      try
        let event = List.assoc id#get_id !objects in
        Logs.debug (fun m -> m "Found game event %d" id#get_id);
        Some (event)
      with
        Not_found ->
        Logs.debug (fun m -> m "NOT found game event %d" id#get_id);
        None

    let get_channel id =
      Core.Std.Option.map (get id) ~f:fst

    let get_update_function id =
      match get id with
      | Some (_,f) -> f
      | None -> failwith "no such ID"
  end

module Challenge_DB =
  struct
    type challenge = (string * string option * int)
    type t = {
        event_listener: unit React.event;
        trigger_event: unit -> unit;
        mutex: Lwt_mutex.t
      }
    let challenges : challenge list ref = ref []

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

    let challenges_to_string () =
      list_to_string string_of_int
      @@ List.map (fun (_,_,id) -> id) !challenges

    let log_debug_challenges () =
      Logs.debug (fun m -> m "challenges: %s" (challenges_to_string ()))

    let load () =
      let event_listener, trigger_event = React.E.create ()
      and mutex = Lwt_mutex.create () in
      { event_listener; trigger_event; mutex}

    let event_listener t = t.event_listener

    let create db challenger ?opponent id =
      log_debug_challenges ();
      challenges := !challenges @ [(challenger, opponent, id#get_id)];
      log_debug_challenges ();
      db.trigger_event ()

    let public_challenges_for_user db user =
      let filter_function = function
        | challenger,None,_ -> user != challenger
        | _ -> false
      in
      let publics = List.filter filter_function
                                !challenges
      in
      List.map (function challenger,_,id -> new id id,challenger) publics

    let private_challenges_for_user db user =
      let filter_function = function
        | _, Some(challengee), _ -> user = challengee
        | _ -> false
      in
      let privates = List.filter filter_function
                                 !challenges
      in
      List.map (function challenger,_,id -> new id id,challenger) privates


    let rec remove_id id =
      let open Ttt_server_lib_types in
      function
        [] -> [], Id_not_present
      | (user,_,id2 as x) :: xs -> if id#get_id = id2 then
                                       xs, Deleted(user)
                                   else
                                     let a,b = remove_id id xs in
                                     x :: a, b

    let remove db id =
      (* TODO: there will be race conditions here, fix *)
      log_debug_challenges ();
      let open Ttt_server_lib_types in
      let list,result = remove_id id !challenges in
      match result with
      | Deleted(user) as d ->
         begin
           challenges := list;
           log_debug_challenges ();
           db.trigger_event ();
           d
         end
      |  Id_not_present -> Id_not_present

    let lock db : <unlock : unit > Lwt.t =
      let%lwt lock = Lwt_mutex.lock db.mutex in
      Lwt.return
        (object
           method unlock = Lwt_mutex.unlock db.mutex
         end)


  end

module Challenge_react_db : Ttt_server_lib_types.CHALLENGE_REACT_DB =
  struct
    let table = Hashtbl.create 10

    let create id =
      let react, update_function = React.E.create () in
      Hashtbl.add table id#get_id update_function;
      react

    let accept id =
      try
        let update_function = Hashtbl.find table id#get_id in
        update_function ();
        Hashtbl.remove table id#get_id
      with
        Not_found -> failwith "internal server error: challenge react db accept"
  end

module HashMapSetInternal
         (KeyType : sig type t end)
         (ValueType : Set.OrderedType)
       :
(sig
  module ValueSet : (Set.S with type elt = ValueType.t)
  type key = KeyType.t
  type value = ValueSet.elt
  type t

  val create : int -> t
  val add : t -> key -> value -> unit
  val get_set : t -> key -> ValueSet.t
  val remove : t -> key -> value -> unit
end
)
  =
  struct
    module ValueSet = Set.Make(ValueType)

    type key = KeyType.t
    type value = ValueSet.elt
    type t = (key, ValueSet.t) Hashtbl.t

    let create : int -> t = Hashtbl.create ~random:false

    let add table x y =
      let current_set =
        try
          Hashtbl.find table x
        with
        | Not_found -> ValueSet.empty
      in
      let newset = ValueSet.add y current_set in
      Hashtbl.replace table x newset

    let get_set table x =
      try
        Hashtbl.find table x
      with
      | Not_found -> ValueSet.empty

    let remove table key value =
      let set = get_set table key in
      Hashtbl.replace table key (ValueSet.remove value set)
  end

module GamesByIdAndUser
         (Games : Set.OrderedType) =
  struct
    type game = Games.t

    module Users =
      struct
        type t = string
        let compare = Pervasives.compare
      end

    module Ids =
      struct
        type t = int
        let compare = Pervasives.compare
      end

    module UsersToIdSet = HashMapSetInternal(Users)(Ids)
    module ValueSet = (UsersToIdSet.ValueSet)

    type t = {
        index1 :  (int, (Games.t * Users.t * Users.t)) Hashtbl.t;
        index2 : UsersToIdSet.t;
      }

    let table = {
        index1 = Hashtbl.create 30;
        index2 = UsersToIdSet.create 30;
      }

    let put_game id user1 user2 game =
      Hashtbl.add table.index1 id#get_id (game, user1, user2);
      UsersToIdSet.add table.index2 user1 id#get_id;
      UsersToIdSet.add table.index2 user2 id#get_id

    let get_game id =
      try
        Some (match Hashtbl.find table.index1 id#get_id with
             game,_,_ -> game)
      with
      | Not_found -> None

    let delete_game id =
      let game,user1,user2 = Hashtbl.find table.index1 id#get_id in
      UsersToIdSet.remove table.index2 user1 id#get_id;
      UsersToIdSet.remove table.index2 user2 id#get_id;
      Hashtbl.remove table.index1 id#get_id

    let opponent_name id user : string option =
      try
        match Hashtbl.find table.index1 id#get_id with
          _,u1,u2 ->
          if user = u1 then
            if user = u2 then
              begin
                Logs.err (fun m ->
                    m "delete_game game %d is between the same player"
                      id#get_id
                  );
                None
              end
            else
              Some(u2)
          else
            if user = u2 then
              Some(u1)
            else
              begin
                Logs.err (fun m ->
                    m "delete_game game %d no player %s"
                      id#get_id
                      user
                  );
                None
              end
      with
      | Not_found -> Logs.err (fun m -> m "delete_game no such id");
                     None

    let get_games_for_user user =
      List.fold_left
        (fun l id ->
          match opponent_name (new id id) user with
          | None -> l
          | Some (opp : string) -> (new id id,opp) :: l
        )
        []
        (ValueSet.elements @@ UsersToIdSet.get_set table.index2 user)
  end

module Users = Ttt_user_lib_users.Make(PostgresDao)

module TTT =
  struct
    include Ttt_game_lib_games.TicTacToeClassical
    let compare = Pervasives.compare
  end

module GameDB = GamesByIdAndUser(TTT)

module MockGameArchiveDB =
  struct
    type game = TTT.t
    let put_game id game =
      Logs.info (fun m -> m "Mock archiving game %d" id#get_id)

    let get_game id =
      None

    let get_games_for_user user = []
  end

module IdGenerator =
  struct
    let current = ref 0

    let next () =
      incr current;
      new id (!current)
  end


module Tic_tac_toe_classical : Ttt_server_lib_types.GAMES
       with type piece = Ttt_game_lib_pieces.XOPiece.t
                           =
  Ttt_server_lib_games.Make
    (Ttt_game_lib_games.TicTacToeClassical)
    (Challenge_DB)
    (IdGenerator)
    (GameRDB)
    (Challenge_react_db)
    (GameDB)
    (MockGameArchiveDB)
    (Users)

let () =
  let open Tic_tac_toe_classical in
  ()


let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ts = Core.Time.(to_string @@ now ()) in
    msgf @@ fun ?header ?tags fmt ->
            Format.kfprintf k ppf ("[%s]%a @[" ^^ fmt ^^ "@]@.")
                            ts Logs.pp_header (level, header)
  in
  { Logs.report = report }

let init_logs () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Debug)

let () = init_logs (); Logs.info (fun m -> m "logs initialized")

let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * Ttt_user_lib_user.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let set_message_next_page m : unit =
  Lwt.async (fun () -> Eliom_reference.set message_next_page (Some m))

let instant_message_ref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope
    (None :
       (string React.event * ((?step:React.step -> string -> unit))) option
    )

(*
let instant_message_react =
  let%lwt event, _ = Eliom_reference.get instant_message_ref in
  Lwt.return event

let send_instant_message m =
  let%lwt _, send = Eliom_reference.get instant_message_ref in
  Lwt.return (send m)
 *)
