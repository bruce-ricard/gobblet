open Ttt_common_lib_types

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
  Logs.debug (fun m ->
      m "Sending challenge %d accept event"
        challenge.id#get_id);
  challenge.update_function ();
  Logs.debug (fun m ->
      m "event sent")

let event challenge =
  challenge.event

let challenger challenge =
  challenge.challenger

let opponent challenge = challenge.opponent

let game_name challenge = challenge.game_name
