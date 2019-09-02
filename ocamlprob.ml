open Devkit

let configfile = ref None

let () =
  begin try
    ExtArg.parse ((ExtArg.may_str "config" configfile "<file> specify config file name") :: Daemon.args);
  with exn -> printfn "Error: %s" (Exn.str exn) end;
  match !configfile with 
  | None -> printfn "Please specify config file. Use -help for help."
  | Some _file ->
    Daemon.manage();
    (* parse config *)
    (* run check according to config *)
    let config_check () =
       Lwt.return_unit
    in
    let main f =
      try%lwt
        Lwt_util.timely_loop ~immediate:false 1. f
      with Daemon.ShouldExit -> Lwt.return_unit
    in
    Lwt_main.run @@ main config_check
