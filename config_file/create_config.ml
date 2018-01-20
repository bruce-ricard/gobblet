let file_name = Sys.argv.(1)

let () = Config_reader.group#write file_name
