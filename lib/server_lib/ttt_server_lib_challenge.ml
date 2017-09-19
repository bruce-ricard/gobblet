open Ttt_common_lib_types
open Internal_types

type t = {
    id : id;
    challenger : string;
    opponent : string option;
    game_name : game_name option;
    event : unit React.event;
    update_function : ?step:React.step -> unit -> unit
  }

let create ?game_name challenger ?opponent id =
  let event, update_function = React.E.create () in
  {
    id;
    challenger;
    opponent;
    game_name;
    event;
    update_function
  }

let id challenge =
  challenge.id

let accept challenge =
  challenge.update_function ()

let event challenge =
  challenge.event

let challenger challenge =
  challenge.challenger

let opponent challenge = challenge.opponent

let game_name challenge = challenge.game_name
