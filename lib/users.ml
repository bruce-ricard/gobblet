type user = string
type password = string

type registration_result =
  | Success
  | UserAlreadyExists
  | Error of string


module type USERS =
  sig
    val register : user -> password -> registration_result
    val check_password : user -> password -> bool
  end

module type USER_DB =
  sig
    type t
    val get : user -> password option
    val put : user -> password -> unit
  end

module Users(User_db : USER_DB) : USERS =
  struct
    let normalize = String.lowercase

    let register user password =
      let user = normalize user in
      match User_db.get user with
        Some _ -> UserAlreadyExists
      | None -> User_db.put user password; Success

    let check_password user password =
      match User_db.get user with
      | None -> false
      | Some pass -> password = pass
  end


module Test_db : USER_DB =
  struct
    type t = (user * password) list ref

    let users = ref [("bruce", "123"); ("arthur", "aeer")]

    let get user =
      try
        Some (List.assoc user !users)
      with
        Not_found -> None

    let put user password = users := (user, password) :: !users
  end

module Users_test = Users(Test_db)
