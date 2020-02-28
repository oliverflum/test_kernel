open Lwt.Infix

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module S = Store.Make (TIME) (PClock)

  let functionality store pclock = 
    let tstr = S.time pclock in
    Logs.info (fun m -> m "functionality-TS: %s" tstr);
    let rec loop = function
      | 200 -> Lwt.return S.Terminate
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
  
  let start _time pclock res (ctx: CON.t) =
    let tstr = S.time pclock in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_name = Key_gen.host_name () in
    let store = new S.webStore ctx res repo token id host_name in
    store#init migration pclock >>= fun _ ->
    let l = S.logic pclock in 
    let f = functionality store pclock in
    Lwt.pick [l;f] >>= fun status -> match status with  
      | S.Suspend -> store#suspend pclock status
      | S.Migrate -> store#suspend pclock status
      | _ -> store#terminate
end
