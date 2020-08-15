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
  let log_level = Some Parsed_config.ServerConfig.log_level in
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level log_level;
  Logs.info (fun m -> m "logs initialized with level \"%s\"" (Logs.level_to_string log_level))

let init () =
  init_logs ();
  Logs.info (fun m -> m "Done with initialization.")
