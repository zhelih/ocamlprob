(** Parse config from Json *)
open ExtLib
open Devkit

let log = Log.from "config"

type comm = Email of string list | Sms of string list | Command of string
type task = Pid of int | Name of string
type report = {
  c : comm;
  t : task;
  period : int;
}

type config = report list

let cache = Hashtbl.create 1

let parse _filename : config =
(*   let json = Yojson.from_file filename in *)
  []

let notdigit = function '0'..'9' -> false | _ -> true
let isnum s =
  try
    String.iter (fun c -> if notdigit c then raise Exit) s;
    true
  with Exit -> false

let update_cache () =
  Hashtbl.clear cache;
  let root = "/proc" in
  let%lwt () = Lwt_unix.files_of_directory root |>
	Lwt_stream.iter_p (fun dir ->
    try%lwt
      if isnum dir then begin
        let dir = Filename.concat root dir in
        let cmdline = Filename.concat dir "cmdline" in
        let cmdline = Std.input_file ~bin:true cmdline in
        let cmdline = String.slice ~last:(String.index cmdline '\000') cmdline in
        if String.length cmdline > 0 then Hashtbl.add cache cmdline 1;
      end;
      Lwt.return_unit
    with _ -> Lwt.return_unit
  ) in
  Lwt.return_unit

let check_task t =
  match t with
  | Pid p -> (try Unix.kill p 0; true with _ -> false)
  | Name n -> begin
    if Sys.win32 then raise (Failure "Cannot find proc by name with Windows flag on");
    try Hashtbl.iter (fun k _ -> if String.exists k n then raise Exit) cache; false with Exit -> true end
