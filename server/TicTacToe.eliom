[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5
    open Html5.D
    open Lwt
    open Ttt

    module XOBoard = Board(XOPiece)

    type messages =
      ((int * int * int) * int * (int * int) * (int * int))
            [@@deriving json]
]

module TicTacToe_app =
  Eliom_registration.App (
    struct
      let application_name = "TicTacToe"
    end)

let board = ref (XOBoard.empty_board ())
let bus = Eliom_bus.create [%derive.json: messages]

let cell id s =
  let cell =
    td
      ~a:[a_class ["cell"]; a_id ("cell" ^ string_of_int id)]
      [pcdata s]
  in
  let _ = [%client
              (Lwt.async (fun () ->
                   let dom_cell = (To_dom.of_element ~%cell) in
                   Lwt_js_events.clicks dom_cell
                                        (fun _ _ ->
                                          dom_cell##.innerHTML := Js.string "X";
                                          Lwt.return ()))
               : unit)]
  in
  cell

let row n a b c =
  tr [cell (3*n + 1) a; cell (3*n + 2) b; cell (3*n + 3) c]

let empty_row n = row n "" "" ""

let board () =
  table [
      empty_row 0;
      empty_row 1;
      empty_row 2
    ]

let page () =
  (html
     (Eliom_tools.F.head
        ~css:[["css"; "TicTacToe.css"]]
        ~title:"Tic Tac Toe"
        ()
     )
     (body
        [
          div [h1 [pcdata "Welcome to this tic tac toe game!"]];
          div [board ()];
        ];
     )
  )


let%client init_client () = ()


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
      Lwt.return (page ())
    )
