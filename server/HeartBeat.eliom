Eliom_registration.Html5.register_service
  ~path:["heartbeat"]
  ~get_params:Eliom_parameter.unit
  (fun _ _ ->
    let open Eliom_content.Html5.D in
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
