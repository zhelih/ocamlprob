(** Parse config from Json *)

type comm = Email of string list | Sms of string list | Command of string
type task = Pid of int | Name of string
type report = {
  c : comm;
  t : task;
  period : int;
}

type config = report list


let parse filename : config =
  let json = Yojson.from_file filename in
  []
