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

let cell id s =
  td ~a:[ a_class ["cell"]; a_id ("cell" ^ string_of_int id)]
     [pcdata s]

let row n a b c =
  tr [cell (3*n + 1) a; cell (3*n + 2) b; cell (3*n + 3) c]

let board =
  table [
      row 0 "X" "X" "";
      row 1 "O" "X" "O";
      row 2 "" "O" "X"
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
    );
  Lwt.async (fun () ->
      let%lwt _ = Lwt_js_events.click (Dom_html.getElementById "cell1") in
      Lwt.return (Eliom_lib.alert "Hello click first cell!")
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
