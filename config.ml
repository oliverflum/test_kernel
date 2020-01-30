open Mirage

let repo =
  let doc = Key.Arg.info ~doc:"URL restore from" ["repo"] in
  Key.(create "repo" Arg.(opt string "" doc))

let token =
  let doc = Key.Arg.info ~doc:"Kernels token for store-repo auth" ["token"] in
  Key.(create "token" Arg.(opt string "" doc))

let main =
  let packages = [ package "cohttp-mirage"; package "duration"; package "yojson" ] in
  foreign
    ~keys:[Key.abstract repo; Key.abstract token]
    ~packages
    "Unikernel.Main" @@ time @-> resolver @-> conduit @-> job

let () =
  let stack = generic_stackv4 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct stack in
  let job =  [ main $ default_time $ res_dns $ conduit ] in
  register "store" job
