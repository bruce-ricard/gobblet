open Game

module type GAMES = functor (Game : GAME) ->
  sig
    val new_game : (player -> string) -> Game.t
    val get_current_games : string -> Game.t list
(*    val get_finished_games : string -> Game.t list
    val get_challenges_sent : string -> Game.t list
    val get_challenges_received : string -> Game.t list*)
  end

module MemoryGames : GAMES = functor (Game : GAME) ->
  struct
    type game =
      {
        game : Game.t;
        players : player -> string
      }

    let g = {
        game = Game.new_game ();
        players = (function P1 -> "bruce" | P2 -> "bruce1")
      }


    let games = ref [g]

    let new_game users = Game.new_game ()

    let get_current_games user =
      (*      List.filter (fun game ->*)
      List.map (fun x -> x.game) !games

  end
