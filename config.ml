open Mirage

let repo =
  let doc = Key.Arg.info ~doc:"URL restore from" ["repo"] in
  Key.(create "repo" Arg.(opt string "" doc))

let token =
  let doc = Key.Arg.info ~doc:"Kernels token for store-repo auth" ["token"] in
  Key.(create "token" Arg.(opt string "" doc))

let migration = 
  let doc = Key.Arg.info ~doc:"True if kernel shall start and idle" ["migration"] in
  Key.(create "migration" Arg.(opt bool false doc))

let id = 
  let doc = Key.Arg.info ~doc:"Mongo ID of the unikernel" ["id"] in
  Key.(create "id" Arg.(opt string "" doc))

let hostid = 
  let doc = Key.Arg.info ~doc:"Mongo ID of the host" ["hostid"] in
  Key.(create "hostid" Arg.(opt string "" doc))

let main =
  let packages = [ package "cohttp-mirage"; package "duration"; package "yojson" ] in
  foreign
    ~keys:[Key.abstract repo; Key.abstract token; Key.abstract migration; Key.abstract id; Key.abstract hostid]
    ~packages
    "Unikernel.Main" @@ time @-> pclock @-> resolver @-> conduit @-> job

let () =
  let stack = generic_stackv4 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct stack in
  let job =  [ main $ default_time $ default_posix_clock $ res_dns $ conduit ] in
  register "store" job
