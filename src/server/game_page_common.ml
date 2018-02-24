let turn_sentence status user : string React.event =
  let user = match user with
      None -> ""
    | Some (user,_) -> user in
  let map = function
    | `GameOver (`Won player) -> player ^ " won the game!"
    | `GameOver `Drawn -> "Draw!"
    | `PlayOn f ->
       begin
         match f user with
         | `Play -> "It's your turn"
         | `Wait -> "It's your oponent's turn"
         | `Watch -> "Enjoy watching " ^ user
       end
  in
  React.E.map map status
