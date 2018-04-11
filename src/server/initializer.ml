let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ts = Core.Time.(to_string @@ now ()) in
    msgf @@ fun ?header ?tags fmt ->
            Format.kfprintf k ppf ("[%s]%a @[" ^^ fmt ^^ "@]@.")
                            ts Logs.pp_header (level, header)
  in
  { Logs.report = report }

let init_logs () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Debug);
  Logs.info (fun m -> m "logs initialized")

let init () =
  init_logs ();
  Logs.info (fun m -> m "Done with initialization.")
