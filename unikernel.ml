open Lwt.Infix

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct
  
  module S = Store.Make (TIME) (PClock)

  let functionality store pclock = 
    let tstr = S.time pclock in
    Logs.info (fun m -> m "Start-TS: %s" tstr);
    let rec loop = function
      | 200 -> Lwt.return false
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
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let store = new S.webStore ctx res repo token in
    store#init migration >>= fun _ ->
    let l = S.logic pclock in 
    let f = functionality store pclock in
    Lwt.pick [l;f] >>= fun suspended ->
    if suspended then store#suspend
    else store#terminate
end
