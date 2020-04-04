open Lwt.Infix

module Status = struct

  type t =
    | Resume
    | Suspend
    | Migrate
    | Terminate

  let string_of_status (var: t) =
    match var with
    | Resume -> "resume"
    | Suspend -> "suspend"
    | Migrate -> "migrate"
    | Terminate -> "terminate"

end

module Make (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) = struct

  let poll_xen_store (path: string) (key: string) (client: OS.Xs.client)  =
  OS.Xs.(immediate client (fun h -> directory h path)) >>= fun dir -> 
  if List.mem key dir then
    OS.Xs.(immediate client (fun h -> read h (path^"/"^key))) >>= fun value ->
    if value <> "" then Lwt.return (Some value)
    else Lwt.return None
  else 
    Lwt.return None

  let time () =
    PClock.now_d_ps () |>
    Ptime.v |>
    Ptime.to_float_s |>
    Float.to_string 

  let read_shutdown_value (client: OS.Xs.client) =
    poll_xen_store "control" "shutdown" client >>= function 
    | Some msg -> begin
      Logs.info (fun m -> m "Got control message: %s" msg);
      match msg with
        | "suspend" -> Lwt.return Status.Suspend
        | "migrate" -> Lwt.return Status.Migrate
        | _ -> Lwt.return Status.Resume
      end 
    | None -> Lwt.return Status.Resume

  let main_loop =
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      read_shutdown_value client
      >>= fun status -> begin 
        match status with
          | Status.Resume -> begin
              TIME.sleep_ns (Duration.of_ms 10) >>= fun _ ->
              inner ()
            end
          | _ -> begin
            let astr = Status.string_of_status status in
            let tstr = time () in
            Logs.info (fun m -> m "%s-TS: %s" astr tstr);
            Lwt.return status
          end
        end
    in inner()

  let steady () =
    Logs.info (fun m -> m "Waiting for go");
    OS.Xs.make () >>= fun client ->
    let rec inner = fun () ->
      poll_xen_store "data" "migrate" client >>= function
      | Some _ -> begin
        let tstr = time () in
        Logs.info (fun m -> m "go-TS: %s" tstr);
        Lwt.return true
      end
      | None -> begin 
        inner ()
      end 
    in 
    inner ()
  end