Eliom_registration.Html.create
  ~path:(Eliom_service.Path ["heartbeat"])
  ~meth:(Eliom_service.Get (Eliom_parameter.unit))
  (fun _ _ ->
    let open Eliom_content.Html.D in
    Lwt.return (
        (html
           (Eliom_tools.F.head
              ~title:"Heartbeat"
              ()
           )
        )
          (body [pcdata "OK"])
      )
  )
