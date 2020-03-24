open Lwt.Infix

module S = Store

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module C = Control.Make (TIME) (PClock)

  let functionality store = 
    let tstr = C.time in
    Logs.info (fun m -> m "functionality-TS: %s" tstr);
    let rec loop = function
      | 200 -> Lwt.return Control.Status.Terminate
      | n ->
        if n = 0 
          then 
            Logs.info (fun m -> m "HELLO... (%i)" n)
          else 
            Logs.info (fun m -> m "AGAIN (%i)" n);
        store#set "count" (S.VInt (n+1));
        TIME.sleep_ns (Duration.of_sec 1) >>= fun () ->
        loop (S.to_int (store#get "count" (S.VInt 0))) 
    in
    loop (S.to_int (store#get "count" (S.VInt 0)))
  
  let start _time _pclock res (ctx: CON.t) =
    let tstr = C.time in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_id = Key_gen.hostid () in
    let store = new S.webStore ctx res repo token id host_id in
    let time = C.time in 
    store#init time migration (C.steady) >>= fun _ ->
    let l = C.main_loop in
    let f = functionality store in
    Lwt.pick [l;f] >>= fun status -> match status with  
      | Control.Status.Suspend -> store#suspend time status
      | Control.Status.Migrate -> store#suspend time status
      | _ -> store#terminate
end
