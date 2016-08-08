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

let%client current_player = ref P1
let%client update_current_player () =
  if !current_player = P1 then
    current_player := P2
  else
    current_player := P1

let%client update_cells_matrix = Array.make_matrix 3 3 (fun (s : string) -> ())

let%client update_game board =
  let position_to_string = function
      None -> ""
    | Some(p) -> XOPiece.to_string p in
  for x = 0 to 2 do
    for y = 0 to 2 do
      update_cells_matrix.(x).(y) (position_to_string board.(x).(y))
    done
  done

let cell x y =
  let cell =
    td
      ~a:[a_class ["cell"]]
      [pcdata ""]
  in
  let _ = [%client
              (Lwt.async (fun () ->
                   let dom_cell = Eliom_content.Html5.To_dom.of_element ~%cell in
                   update_cells_matrix.(~%x).(~%y) <-
                     (fun s -> dom_cell##.innerHTML := Js.string s);
                   Lwt_js_events.clicks
                     dom_cell
                     (fun _ _ ->
                       let open XOBoard in
                       (
                       match XOBoard.move !(~%board) ~row:~%x ~column:~%y !current_player with
                         InvalidMove -> Eliom_lib.alert "Invalid move!"
                       | Next(result, new_board) ->
                          begin
                            update_game new_board;
                            ~%board := new_board;
                            match result with
                              KeepPlaying ->  update_current_player ()
                            | Won P1 -> Eliom_lib.alert "Player 1 won !"
                            | Won P2 -> Eliom_lib.alert "Player 2 won !"
                            | Draw -> Eliom_lib.alert " Draw!"
                          end
                       );
                       Lwt.return ()))
               : unit)]
  in
  cell

let row x =
  tr [cell x 0 ; cell x 1; cell x 2]

let empty_row n = row n

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
