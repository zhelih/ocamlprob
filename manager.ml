(** Create and remove Lwt task to look for jobs *)
open Devkit

let log = Log.from "manager"

let universe_number = ref 0 (* if task was launched under different universe number, it terminates *)

let cancel_all_tasks () =
  incr universe_number

(*TODO move to separate module? *)
let send_email is_running proc email =
  log #info "Sending email of %s to %s" (Config.show_proc proc) email;
  let open Async_smtp in
  Core_kernel.Or_error.ok_exn @@ Async_unix.Thread_safe.block_on_async_exn @@ begin fun () ->
  Simplemail.send
  ~to_:[Email_address.of_string_exn email]
  ~subject:"Ocamlprob Alert"
  (Simplemail.Content.text @@ Printf.sprintf "ALERT: Host %s : Ocamlprob detected task %s %s."
      (Unix.gethostname ())
      (Config.show_proc proc)
      (if is_running then "RUNNING" else "STOPPED") )
  end;
  Lwt.return_unit

let send_report is_running proc comm =
  match comm with
  | Config.Email e -> send_email is_running proc e
  | Config.Phone p -> log #warn "Cannot message number %s. Not implemented" p; Lwt.return_unit
  | Config.Command c -> let%lwt _ = Lwt_unix.system c in Lwt.return_unit

let launch_tasks config =
  let open Config in
  let tasks = List.map (fun t ->
    let rec loop un st =
      if !universe_number == un then begin
        let is_running = Config.check_task t.p in
        let%lwt () = if is_running <> st then Lwt_list.iter_p (send_report is_running t.p) t.c else Lwt.return_unit in
        let%lwt () = Lwt_unix.sleep @@ float t.period in
        loop un is_running
      end else Lwt.return_unit
    in
    loop !universe_number false
  ) config in
  Lwt_util.async (fun () -> Lwt.join tasks);
  ()
