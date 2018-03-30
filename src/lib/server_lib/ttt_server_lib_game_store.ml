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

module type API_GAME_TYPES =
  sig
    type tttc
    type tttxo
    type three_men_morris
  end

module GamesByIdAndUser(ApiGameTypes : API_GAME_TYPES) =
  struct
    include ApiGameTypes

    type ngame = (tttc, tttxo, three_men_morris)
                   Ttt_server_lib_types.named_game

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
      let open Ttt_common_lib_types in
      List.fold_left
        (fun l id ->
          match opponent_name (new id id) user with
          | None -> l
          | Some (opp : string) -> (new id id,opp) :: l
        )
        []
        (ValueSet.elements @@ UsersToIdSet.get_set table.index2 user)
  end
