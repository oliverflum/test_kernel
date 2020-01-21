open Lwt.Infix
open Mirage_types_lwt

module Main (T: TIME) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct
  
  let functionality store = 
    let rec loop = function
      | 1000 -> Lwt.return false
      | n ->
        if n = 0 
          then 
            Logs.info (fun m -> m "HELLO... (%i)" n)
          else 
            Logs.info (fun m -> m "AGAIN (%i)" n);
        store#set "count" (Store.VInt (n+1));
        T.sleep_ns (Duration.of_sec 1) >>= fun () ->
        loop (Store.to_int (store#get "count" (Store.VInt 0))) 
    in
    loop (Store.to_int (store#get "count" (Store.VInt 0)))

  let logic =
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      OS.Xs.(immediate client (fun h -> directory h "control")) >>= fun dir -> 
      begin if List.mem "shutdown" dir 
        then begin
          OS.Xs.(immediate client (fun h -> read h "control/shutdown")) >>= fun msg ->
          if msg <> "" then 
            Logs.info (fun m -> m "Got control message: %s" msg);
          match msg with
          | "suspend" -> 
            Lwt.return true
          | "poweroff" -> 
            OS.Sched.shutdown OS.Sched.Poweroff;
            Lwt.return false (* Doesn't get here! *)
          | "reboot" ->
            OS.Sched.shutdown OS.Sched.Reboot;
            Lwt.return false (* Doesn't get here! *)
          | "halt" ->
            OS.Sched.shutdown OS.Sched.Poweroff;
            Lwt.return false
          | "crash" ->
            OS.Sched.shutdown OS.Sched.Crash;
            Lwt.return false
          | _ -> 
            Lwt.return false
        end 
      else Lwt.return false end >>= fun suspend ->
      if suspend then 
        Lwt.return true
      else begin
        T.sleep_ns (Duration.of_sec 1) >>= fun _ ->
        inner ()
      end
    in inner ()
  
  let start _time res (ctx:CON.t) =
    let store = new Store.webStore ctx res 
      (Key_gen.repo ()) (Key_gen.uuid ()) (Key_gen.password ()) in
    store#init >>= fun _ ->
    let l = logic in 
    let f = functionality store in
    Lwt.pick [l;f] >>= fun suspended ->
    if suspended then store#suspend
    else begin
      OS.Sched.shutdown OS.Sched.Poweroff;
      Lwt.return ()
    end
end
