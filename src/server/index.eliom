open Eliom_content.Html5.D

let welcome_page () =
  let content =
    [
      pcdata "Welcome! To start playing, click Play in the menu."
    ] in
  Base.skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Board games"
    content


let () = Eliom_registration.Html5.register
    ~service:Services.main_service
    (fun () () ->
      welcome_page ()
    )