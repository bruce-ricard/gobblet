open Ttt_common_lib_types
open Internal_types

type t
val create : ?game_name:game_name -> string -> ?opponent:string -> id -> t
val challenger : t -> string
val opponent : t -> string option
val id : t -> id
val accept : t -> unit
val event : t -> unit React.event
val game_name : t -> game_name option
