let pieces_path = "static/wood-pieces.png"

let register () =
  Eliom_registration.File.register
    ~service:Services.image_piece_service
    (fun () () ->
      Lwt.return pieces_path
    )

let () =
  register ()
