class user login password =
object
  val login = (login : string)
  val password = password
  val mutable logged_in = true

  method normalized_name = login
end
