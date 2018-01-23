module type CONFIG =
  sig
    val host : string option
    val port : int option
    val user : string option
    val password : string option
    val database : string option
  end

module EmptyConfig : CONFIG =
  struct
    let host = None
    let port = None
    let user = None
    let password = None
    let database = None
  end
