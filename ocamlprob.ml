open Devkit

let configfile = ref None

let () =
  begin try
    ExtArg.parse ((ExtArg.may_str "config" configfile "<file> specify config file name") :: Daemon.args);
  with exn -> printfn "Error: %s" (Exn.str exn) end;
  match !configfile with 
  | None -> printfn "Please specify config file. Use -help for help."
  | Some file -> (printfn "Got file %s" file; Daemon.manage())
