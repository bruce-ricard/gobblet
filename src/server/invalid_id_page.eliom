open Eliom_content.Html5.D

let html () =
  Base.skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"XOs"
    [
      pcdata "This game doesn't exist. Go to your ";
      a Services.show_my_games_service [pcdata "game list"] ();
      pcdata " and chose a game from there."
    ]
