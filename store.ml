open Lwt.Infix

exception UnsupportedType
exception WrongType of string
module StringMap = Map.Make(String)
module JS = Yojson.Basic

module Make (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) = struct

  type vtype = 
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VString of string
    | VList of vtype list
  
  type status =
    | Resume
    | Suspend
    | Migrate
    | Terminate
  
  let type_of_action (var: status) =
    match var with
    | Resume -> "resume"
    | Suspend -> "suspend"
    | Migrate -> "migrate"
    | Terminate -> "terminate"

  let typeof (var: vtype) =
    match var with
    | VBool _ -> "bool"
    | VFloat _ -> "float"
    | VInt _ -> "int"
    | VString _ -> "string"
    | VList _ -> "array"
  
  let to_int (var: vtype) =
    match var with
    | VInt i -> i
    | _ -> raise (WrongType ("Expected integer got "^typeof var))
  
  let to_str (var: vtype) =
    match var with
    | VString s -> s
    | _ -> raise (WrongType ("Expected string got "^typeof var))
  
  let to_float (var: vtype) =
    match var with
    | VFloat f -> f
    | _ -> raise (WrongType ("Expected float got "^typeof var))
  
  let to_bool (var: vtype) =
    match var with
    | VBool b -> b
    | _ -> raise (WrongType ("Expected boolean got "^typeof var))
  
  let conv_each_var f (vlist: vtype) =
    match vlist with
    | VList l -> List.map f l
    | _ -> raise (WrongType "Must pass a list to use this function")
  
  let rec vtype_to_json (var: vtype) =
    match var with
    | VBool b -> `Bool b
    | VFloat f -> `Float f
    | VInt i -> `Int i
    | VString s -> `String s
    | VList l -> `List (List.map vtype_to_json l) 
      
  let rec json_to_vtype (js: JS.t) =
    match js with
    | `Bool b -> VBool b
    | `Float f -> VFloat f
    | `Int i -> VInt i
    | `String s -> VString s
    | `List l -> VList (List.map json_to_vtype l) 
    | _ -> raise UnsupportedType
  
  let poll_xen_store (path: string) (key: string) (client: OS.Xs.client)  =
    OS.Xs.(immediate client (fun h -> directory h path)) >>= fun dir -> 
    if List.mem key dir then
      OS.Xs.(immediate client (fun h -> read h (path^"/"^key))) >>= fun value ->
      if value <> "" then Lwt.return (Some value)
      else Lwt.return None
    else 
      Lwt.return None
  
  let time pclock =
    PClock.now_d_ps pclock |>
    Ptime.v |>
    Ptime.to_float_s |>
    Float.to_string 

  let read_shutdown_value client =
    poll_xen_store "control" "shutdown" client >>= function 
    | Some msg -> begin
      Logs.info (fun m -> m "Got control message: %s" msg);
      match msg with
        | "suspend" -> Lwt.return Suspend
        | "migrate" -> Lwt.return Migrate
        | _ -> Lwt.return Resume
      end 
    | None -> Lwt.return Resume

  let logic pclock =
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      read_shutdown_value client
      >>= fun status -> begin 
        match status with
          | Resume -> begin
              TIME.sleep_ns (Duration.of_ms 10) >>= fun _ ->
              inner ()
            end
          | _ -> begin
            let astr = type_of_action status in
            let tstr = time pclock in
            Logs.info (fun m -> m "%s-TS: %s" astr tstr);
            Lwt.return status
          end
        end
    in inner()

  let check_control_status =
    OS.Xs.make () >>= fun client ->
    read_shutdown_value client

  let steady pclock = 
    Logs.info (fun m -> m "Waiting for go");
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      poll_xen_store "data" "migrate" client >>= function 
      | Some _ -> begin
        let tstr = time pclock in
        Logs.info (fun m -> m "go-TS: %s" tstr);
        Lwt.return true
      end 
      | None -> begin 
        inner ()
      end 
    in 
    inner ()
  
  class webStore ctx resolver repo token id host_id = 
    let ctx = Cohttp_mirage.Client.ctx resolver ctx in
    object (self)
      val store_ctx = ctx
      val repo = repo
      val token = token
      val id = id
      val host_id = host_id
      val mutable map = StringMap.empty
  
      method private post_ready =
        let path = "/hosts/"^host_id^"/unikernels/"^id^"/ready" in
        let uri = Uri.of_string (repo ^ path) in
        let headers = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        Cohttp_mirage.Client.post ~ctx:store_ctx ~headers uri >>= fun (response, _) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then begin
          Logs.info (fun m -> m "Notified MirageManager of being ready");
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not notify MirageManager of being ready: %n" code);
          Lwt.return false 
        end 

      method private get_state pclock =
        let path = "/hosts/"^host_id^"/unikernels/"^id^"/states/latest" in
        let uri = Uri.of_string (repo ^ path) in
        let headers = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        Cohttp_mirage.Client.get ~ctx:store_ctx ~headers uri >>= fun (response, body) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        if code == 200 then begin
          let tstr = time pclock in
          Logs.info (fun m -> m "Got state: %s at %s" body_str tstr);
          let json = JS.from_string body_str in 
          let state = JS.Util.member "state" json in
          self#store_all state;
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not retrieve state: %n" code);
          Lwt.return false 
        end
  
      method private post_state pclock status =
        let path = "/hosts/"^host_id^"/unikernels/"^id^"/states" in
        let uri = Uri.of_string (repo ^ path) in
        let body_str = self#create_state_body status in
        let body = Cohttp_lwt.Body.of_string body_str in
        let h1 = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        let headers = Cohttp.Header.add h1 "Content-Type" "application/json" in
        Cohttp_mirage.Client.post ~ctx:store_ctx ~body ~headers uri >>= fun (response, _) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then begin
          let tstr = time pclock in
          Logs.info (fun m -> m "Wrote state to repo at %s" tstr);
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not write state: %n" code);
          Lwt.return false 
        end 
      
      method private post_terminate =
        let path = "/hosts/"^host_id^"/unikernels/"^id^"/terminate" in
        let uri = Uri.of_string (repo ^ path) in
        let h1 = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        let headers = Cohttp.Header.add h1 "Content-Type" "application/json" in
        Cohttp_mirage.Client.post ~ctx:store_ctx ~headers uri >>= fun (response, _) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then begin
          Logs.info (fun m -> m "Notified MirageManager of terminating");
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not notify MirageManager of temriating %n. Unikernel will be shown alive in Manager" code);
          Lwt.return false 
        end
      
      method private create_state_body status =
        let js_map = StringMap.map (fun v -> (vtype_to_json v)) map in
        let l = List.of_seq (StringMap.to_seq js_map) in
        let keys =  [("state", `Assoc l)] in
        let keys = match status with 
          | Suspend -> ("action", `String "suspend") :: keys 
          | Migrate -> ("action", `String "migrate") :: keys  
          | _ -> keys
        in
        let json = `Assoc keys in
        JS.pretty_to_string json
  
      method private store_all (json: Yojson.Basic.t) =
        let keys = JS.Util.keys json in
        let values = List.map json_to_vtype (JS.Util.values json) in
        List.iter2 self#set keys values
  
      method get (key: string) (def: vtype) =  
        try 
          StringMap.find key map
        with Not_found -> 
          self#set key def;
          def
  
      method set (key: string) (value: vtype) =
        map <- StringMap.add key value map
  
      method terminate = 
        Logs.info (fun m -> m "Terminated");
        if token <> "" then begin
          self#post_terminate >>= fun _ ->
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end else begin
          Logs.info (fun m -> m "Not logged in to Repo. Shutting down without terminating on server");
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end
  
      method suspend pclock status =
        Logs.info (fun m -> m "Suspended");
        if token <> "" then begin
          self#post_state pclock status >>= fun _ ->
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end else begin
          Logs.info (fun m -> m "Not logged in to Repo. Shutting down without suspending state");
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end
        
      method init (migration: bool) pclock =
        Logs.info (fun m -> m "Started");
        if repo <> "" then begin
          Logs.info (fun m -> m "Using repo: %s" repo);
          if migration then begin
            self#post_ready >>= fun _ ->
            steady pclock >>= fun _ -> 
            self#get_state pclock >>= fun _ ->
            Lwt.return true
          end else begin
            self#get_state pclock >>= fun _ ->
            Lwt.return true
          end
        end
        else begin 
          Logs.info (fun m -> m "Not using a repo.");
          Lwt.return false
        end
    end 
end

