open Devkit

let configfile = ref None

type t_state = NoFile | LastMod of float

let state = ref NoFile
let config = ref []
let log = Log.from "main"

let () =
  begin try
    ExtArg.parse ((ExtArg.may_str "config" configfile "<file> specify config file name") :: Daemon.args);
  with exn -> printfn "Error: %s" (Exn.str exn) end;
  match !configfile with 
  | None -> printfn "Please specify config file. Use -help for help."
  | Some file ->
    Daemon.manage();
    (* parse config *)
    (* run check according to config *)
    let config_check () =
      try%lwt
        match%lwt Lwt_unix.file_exists file with
        | false ->
          begin match !state with
          | LastMod _ ->
            log #info "No config file, removing all checks";
            Manager.cancel_all_tasks ();
            state := NoFile;
            Lwt.return_unit
          | NoFile -> Lwt.return_unit
          end
        | true ->
          let%lwt st = Lwt_unix.stat file in
          match !state with
          | LastMod s when s >= st.Unix.st_mtime -> Lwt.return_unit
          | _ ->
            log #info "Config update";
            config := Config.parse file;
            Manager.cancel_all_tasks ();
            Manager.launch_tasks !config;
            state := (LastMod st.Unix.st_mtime);
            Lwt.return_unit
      with exn -> (log #warn ~exn "Error"; Lwt.return_unit)
    in
    let main f =
      try%lwt
        let config_check = Lwt_util.timely_loop ~immediate:true 1. f in (*FIXME period*)
        let cache_update = Lwt_util.timely_loop 1. Config.update_cache in
        Lwt.join [cache_update; config_check]
      with Daemon.ShouldExit -> Lwt.return_unit
    in
    Lwt_main.run @@ main config_check
