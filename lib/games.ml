open Game

module type GAMES =
  functor (Game : GAME) (Piece : Ttt.PIECE) ->
  sig
    val new_game : (player -> string) -> int * Game(Piece).t
    val get_current_games : string -> (int * Game(Piece).t) list
    val get_game_by_id : int -> Game(Piece).t option
(*    val get_finished_games : string -> Game.t list
    val get_challenges_sent : string -> Game.t list
    val get_challenges_received : string -> Game.t list*)
  end

module MemoryGames : GAMES =
  functor (Game : GAME) (Piece : Ttt.PIECE) ->
  struct
  module Game = Game(Piece)
    type game =
      {
        game : Game.t;
        players : player -> string
      }

    let g = {
        game = Game.new_game ();
        players = (function P1 -> "bruce" | P2 -> "bruce1")
      }


    let games = ref [1,g]

    let next_id =
      let id = ref 2 in
      function () ->
               incr id;
               !id

    let new_game players =
      let game = Game.new_game ()
      and id = next_id () in
      games := (id,{game ; players}) :: ! games;
      id,game

    let get_current_games user =
      (*      List.filter (fun game ->*)
      List.map (fun (id,x) -> id,x.game) !games

    let get_game_by_id id =
      try
        Some (List.assoc id !games).game
      with
        Not_found -> None

  end
