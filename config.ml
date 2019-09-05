(** Parse config from Json *)
open ExtLib
open Devkit

let log = Log.from "config"

type comm = Email of string | Phone of string | Command of string [@@deriving show {with_path = false}]
type proc = Pid of int | Name of string [@@deriving show {with_path = false}]
type task = {
  c : comm list;
  p : proc;
  period : int;
} [@@deriving show {with_path = false}]

type config = task list [@@deriving show {with_path = false}]

let cache = Hashtbl.create 1

let parse filename : config =
  try
    let json = Yojson.Basic.from_file filename in
    let open Yojson.Basic.Util in
    let json = member "config" json in
    let comms = json |> member "comms" |> to_assoc |> List.map (fun (name, j) ->
      let get_maybe_null str j = if j == `Null || `Null = member str j then [] else filter_string @@ to_list @@ member str j in
      let emails = get_maybe_null "emails" j in
      let phones = get_maybe_null "phones" j in
      let cmds   = get_maybe_null "cmds"   j in
      let all = [ List.map (fun e -> Email e) emails; List.map (fun p -> Phone p) phones; List.map (fun c -> Command c) cmds ] in
      let all = List.concat all in
      name, all
    ) in
    json |> member "tasks" |> to_list |> List.map (fun j ->
      let period = j |> member "period" |> to_int in
      let comm = j |> member "comm" |> to_string in
      let c = try List.assoc comm comms with _ -> Exn.fail "unknown comm %S" comm in
      let p = match member "name" j, member "pid" j with
      | `Null, `Null -> Exn.fail "Missing name/task (comm = %s, period = %d)" comm period
      | `Null, p -> Pid (to_int p)
      | n, `Null -> Name (to_string n)
      | _, _ -> Exn.fail "Malformed task" (*TODO more details? *)
      in
      { c; p; period }
    )
  with exn -> log #warn ~exn "Error"; []

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
