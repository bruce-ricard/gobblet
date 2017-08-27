open Ttt_user_lib_types

type user = string
type password = string

module type USERS =
  sig
    val register : user -> password -> registration_result
    (*    val get_user : user -> Ttt_user_lib_user.user option*)
    val log_in : user -> password -> Ttt_user_lib_user.user option (* TODO maybe make this a logged in user ?*)
    val exists : user -> bool
  end

module type USER_DB =
  sig
    val get : user -> Sha256.t -> bool
    val exists : user -> bool
    val put : user -> Sha256.t -> bool
  end

module Make(User_db : USER_DB) : USERS =
  struct
    let normalize_username = String.lowercase
    let hash_password username password =
      let random_string = "mqpKXje8Jzpjgh3#cNmAq2-Uihx" in
      let string_to_hash = random_string ^ username ^ password in
      Sha256.string string_to_hash

    let is_valid_username username =
      let username_regexp = Str.regexp "^[A-Za-z0-9_-]+$"
      and length = String.length username in
      length >= 3 && length <= 30 &&
        Str.string_match username_regexp username 0

    let register user password =
      let user = normalize_username user in
      if User_db.exists user then
        UserAlreadyExists
      else if not (is_valid_username user) then
        InvalidUsername
      else if User_db.put user (hash_password user password) then
        Success
      else
        Error("Error while adding new user")

    let log_in user password =
      let user = normalize_username user in
      let password_hash = hash_password user password in
      if (User_db.get user password_hash) then
        Some (new Ttt_user_lib_user.user user password)
      else
        None

(*    let get_user user =
      User_db.get user ""*)

    let exists = User_db.exists
  end

module Test_db : USER_DB =
  struct
    type t = (user * Sha256.t) list ref

    let users = []
    let users_o = ref ((List.map (fun (name, passwd) ->
                            (name, passwd), new Ttt_user_lib_user.user name passwd)) users)

    let exists login =
      List.exists (fun ((l,_),_) -> l = login) !users_o

    let get user password =
      try
        ignore (List.assoc (user, password) !users_o);
        true
      with
        Not_found -> false

    let put login password =
      users_o := ((login, password), new Ttt_user_lib_user.user login password) :: !users_o;
      true
  end

(*module Users_test = Make(Test_db)*)
    (*module UsersM = Make(PostgresDao)*)
