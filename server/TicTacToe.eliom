[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

    type messages =
      ((int * int * int) * int * (int * int) * (int * int))
            [@@deriving json]
]

module TicTacToe_app =
  Eliom_registration.App (
    struct
      let application_name = "TicTacToe"
    end)

let bus = Eliom_bus.create [%derive.json: messages]

let canvas_elt =
  canvas ~a:[a_width 300; a_height 300]
             [pcdata "your browser doesn't support canvas"]

let cell s =
  td ~a:[ a_class ["cell"]]
     [pcdata s]

let row a b c =
  tr [cell a; cell b; cell c]

let board =
  table [
      row "X" "X" "";
      row "O" "X" "O";
      row "" "O" "X"
    ]

let page =
  (html
     (Eliom_tools.F.head
        ~css:[["css"; "TicTacToe.css"]]
        ~title:"Tic Tac Toe"
        ()
     )
     (body
        [
          h1 [pcdata "Welcome to this tic tac toe game!"];
          board;
          canvas_elt
        ];


     )
  )


let%client init_client () =
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";
  ctx##.fillStyle := Js.string "#FF0000";
  ctx##fillRect 0. 0. 300. 300.;
  Lwt.async (fun () ->
      let%lwt _ = Lwt_js_events.click  Dom_html.document in
      Lwt.return (Eliom_lib.alert "Hello click!")
    )



let main_service =
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

let () =
  TicTacToe_app.register
    ~service:main_service
    (fun () () ->
      let _ = [%client (init_client () : unit)] in
      Lwt.return page
    )


    (*let%client _ = Eliom_lib.alert "Hello!"*)
