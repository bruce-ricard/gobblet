open Ttt_common_lib_types

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

module Dao = UsersPostgresDao.Make(Config_parser.PostgresConfig)

module GameList = Ttt_server_lib_game_list.Make(Dao)

module TicTacToeClassical =
  GameList.TicTacToeClassical

module TicTacToeXOnly =
  GameList.TicTacToeXOnly

module ThreeMenMorris =
  GameList.ThreeMenMorris

module ApiGameTypes =
  struct
    type tttc = TicTacToeClassical.game
    type tttxo = TicTacToeXOnly.game
    type three_men_morris = ThreeMenMorris.game

    type ngame = (tttc, tttxo, three_men_morris)
                   Ttt_server_lib_types.named_game
  end

module DbGameTypes =
  struct
    type tttc = GameList.TTTCI.t
    type tttxo = GameList.TTTXOI.t
    type three_men_morris = GameList.ThreeMenMorrisInternal.t

    type ngame = (tttc, tttxo, three_men_morris) Ttt_server_lib_types.named_game
  end

module Challenge_DB : Ttt_server_lib_types.CHALLENGES =
  struct
    type challenge = Ttt_server_lib_challenge.t

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
      list_to_string (fun i -> string_of_int (i#get_id))
      @@ List.map Ttt_server_lib_challenge.id !challenges

    let log_debug_challenges () =
      Logs.debug (fun m -> m "challenges: %s" (challenges_to_string ()))

    let load () =
      let event_listener, trigger_event = React.E.create ()
      and mutex = Lwt_mutex.create () in
      { event_listener; trigger_event; mutex}

    let event_listener t = t.event_listener

    let create db challenger ?opponent game_name id =
      let challenge =
        Ttt_server_lib_challenge.create ?game_name challenger ?opponent id in
      log_debug_challenges ();
      challenges :=
        !challenges @ [challenge];
      log_debug_challenges ();
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
                                !challenges
      in
      publics

    let private_challenges_for_user db user =
      let filter_function challenge =
        match Ttt_server_lib_challenge.opponent challenge with
          Some(opp) -> user = opp
        | _ -> false
      in
      let privates = List.filter filter_function
                                 !challenges
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
      log_debug_challenges ();
      let open Ttt_server_lib_types in
      let list,result = remove_id id !challenges in
      match result with
      | Deleted(user) as d ->
         begin
           challenges := list;
           log_debug_challenges ();
           (*db.trigger_event ();*)
           d
         end
      |  Id_not_present -> Id_not_present

    let lock db : <unlock : unit > Lwt.t =
      let%lwt lock = Lwt_mutex.lock db.mutex in
      Lwt.return
        (object
           method unlock = Lwt_mutex.unlock db.mutex
         end)

    let send_updates db =
      let trigger = db.trigger_event in
      let rec aux () =
        let open Lwt in
        Lwt.async (fun () ->
            let%lwt () = Lwt_unix.sleep 3. in
            trigger ();
            Lwt.return (aux())
          )
      in
      aux ()
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

module GamesByIdAndUser =
  struct
    include ApiGameTypes

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
        index1 :  (int, (ngame * Users.t * Users.t)) Hashtbl.t;
        index2 : UsersToIdSet.t;
      }

    let table = {
        index1 = Hashtbl.create 30;
        index2 = UsersToIdSet.create 30;
      }

    let (put_game : Ttt_common_lib_types.id ->
                    string -> string -> ngame -> unit) =
      fun id user1 user2 game ->

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

module Users = Ttt_user_lib_users.Make(Dao)

module TTT =
  struct
    type t = GameList.TicTacToeClassical.game
    let compare = Pervasives.compare
  end

module TTTXonly =
  struct
    include GameList.TTTXOI
    let compare = Pervasives.compare
  end

module MockGameArchiveDB =
  struct
    include DbGameTypes

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

module Games : Ttt_server_lib_types.GAMES
       with type tttc = GamesByIdAndUser.tttc
        and type tttxo = GamesByIdAndUser.tttxo
        and type three_men_morris = GamesByIdAndUser.three_men_morris =
  Ttt_server_lib_games.Make
    (Challenge_DB)
    (IdGenerator)
    (MockGameArchiveDB)
    (GamesByIdAndUser)
    (TicTacToeClassical)
    (TicTacToeXOnly)
    (ThreeMenMorris)
    (Users)

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
