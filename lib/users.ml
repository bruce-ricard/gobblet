open Types

type user = string
type password = string

module type USERS =
  sig
    val register : user -> password -> registration_result
    (*    val get_user : user -> User.user option*)
    val log_in : user -> password -> User.user option (* TODO maybe make this a logged in user ?*)
  end

module type USER_DB =
  sig
    val get : user -> password -> bool
    val exists : user -> bool
    val put : user -> password -> bool
  end

module Make(User_db : USER_DB) : USERS =
  struct
    let normalize = String.lowercase

    let register user password =
      let user = normalize user in
      match User_db.exists user with
        true -> UserAlreadyExists
      | false -> if User_db.put user password then
                   Success
                 else
                   Error("Error while adding new user")

    let log_in user password =
      if (User_db.get (normalize user) password) then
        Some (new  User.user user password)
      else
        None

    let get_user user =
      User_db.get user ""
  end

module Test_db : USER_DB =
  struct
    type t = (user * password) list ref

    let users = [("bruce", ""); ("bruce2", ""); ("bruce3", ""); ("arthur", "")]
    let users_o = ref ((List.map (fun (name, passwd) ->
                            (name, passwd), new User.user name passwd)) users)

    let exists login =
      List.exists (fun ((l,_),_) -> l = login) !users_o

    let get user password =
      try
        ignore (List.assoc (user, password) !users_o);
        true
      with
        Not_found -> false

    let put login password =
      users_o := ((login, password), new User.user login password) :: !users_o;
      true
  end

module Users_test = Make(Test_db)
