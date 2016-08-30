type user = string
type password = string

type registration_result =
  | Success
  | UserAlreadyExists
  | Error of string

module type USERS =
  sig
    val register : user -> password -> registration_result
    val get_user : user -> password -> User.user option
  end

module type USER_DB =
  sig
    val get : user -> password -> User.user option
    val exists : user -> bool
    val put : user -> User.user  -> unit
  end

module Users(User_db : USER_DB) : USERS =
  struct
    let normalize = String.lowercase

    let register user password =
      let user = normalize user in
      match User_db.exists user with
        true -> UserAlreadyExists
      | false -> User_db.put user (new User.user user password); Success

    let get_user  user password =
      User_db.get user password
  end


module Test_db : USER_DB =
  struct
    type t = (user * password) list ref

    let users = [("bruce", "123"); ("bruce2", "123"); ("bruce3", "123"); ("arthur", "aeer")]
    let users_o = ref ((List.map (fun (name, passwd) -> name, new User.user name passwd)) users)

    let exists login =
      List.exists (fun (l,_) -> l = login) !users_o

    let get user password =
      try
        Some (List.assoc user !users_o)
      with
        Not_found -> None

    let put login user = users_o := (login, user) :: !users_o
  end

module Users_test = Users(Test_db)
