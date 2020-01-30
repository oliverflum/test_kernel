open Lwt.Infix

exception UnsupportedType
exception WrongType of string
module StringMap = Map.Make(String)
module JS = Yojson.Basic

module Make (T: Mirage_time.S) = struct 
  type vtype = 
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VString of string
    | VList of vtype list
  
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
    
  let logic =
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      poll_xen_store "control" "shutdown" client >>= function 
        | Some msg -> begin
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
        | None -> Lwt.return false 
      >>= fun suspend ->
      if suspend then 
        Lwt.return true
      else begin
        T.sleep_ns (Duration.of_sec 1) >>= fun _ ->
        inner ()
      end
    in inner ()

  let steady = 
    Logs.info (fun m -> m "Waiting for go");
    OS.Xs.make () >>= fun client ->
    let rec inner () = 
      poll_xen_store "data" "migrate" client >>= function 
        | Some _ -> begin
          Logs.info (fun m -> m "Migration go");
          Lwt.return true
          end 
        | None -> begin 
          inner ()
        end
    in inner ()
  
  class webStore ctx resolver repo token = 
    let ctx = Cohttp_mirage.Client.ctx resolver ctx in
    object (self)
      val store_ctx = ctx
      val repo = repo
      val token = token
      val mutable map = StringMap.empty
  
      method private get_store =  
        let uri = Uri.of_string (repo ^ "/store") in
        let headers = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        Cohttp_mirage.Client.get ~ctx:store_ctx ~headers uri >>= fun (response, body) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        if code == 200 then begin
          Logs.info (fun m -> m "Got store: %s" body_str);
          let json = JS.from_string body_str in 
          let store = JS.Util.member "store" json in
          self#store_all store;
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not retrieve store: %n" code);
          Lwt.return false 
        end
  
      method private post_store =
        let uri = Uri.of_string (repo ^ "/store") in
        let body_str = self#map_to_json_string in
        let body = Cohttp_lwt.Body.of_string body_str in
        let h1 = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        let headers = Cohttp.Header.add h1 "Content-Type" "application/json" in
        Cohttp_mirage.Client.post ~ctx:store_ctx ~body ~headers uri >>= fun (response, _) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then begin
          Logs.info (fun m -> m "Wrote store to repo");
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not write store: %n" code);
          Lwt.return false 
        end 
      
      method private post_terminate =
        let uri = Uri.of_string (repo ^ "/unikernel/terminate") in
        let h1 = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
        let headers = Cohttp.Header.add h1 "Content-Type" "application/json" in
        Cohttp_mirage.Client.post ~ctx:store_ctx ~headers uri >>= fun (response, _) ->
        let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then begin
          Logs.info (fun m -> m "Terminated self in manager");
          Lwt.return true
        end else begin
          Logs.info (fun m -> m "Could not terminate self: %n. Unikernel will be shown alive in Manager" code);
          Lwt.return false 
        end 
      
      method private map_to_json_string =
        let js_map = StringMap.map (fun v -> (vtype_to_json v)) map in
        let l = List.of_seq (StringMap.to_seq js_map) in
        let json = `Assoc [("store", `Assoc l)] in
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
        if token <> "" then begin
          self#post_terminate >>= fun _ ->
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end else begin
          Logs.info (fun m -> m "Not logged in to Repo. Shutting down without terminating on server");
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end
  
      method suspend = 
        if token <> "" then begin
          self#post_store >>= fun _ ->
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end else begin
          Logs.info (fun m -> m "Not logged in to Repo. Shutting down without suspending state");
          OS.Sched.shutdown OS.Sched.Poweroff;
          Lwt.return ()
        end
        
      method init (migration: bool) = 
        if repo <> "" then begin
          Logs.info (fun m -> m "Using repo: %s" repo);
          if migration then begin
            steady >>= fun _ -> 
            self#get_store >>= fun _ ->
            Lwt.return true
          end else begin
            self#get_store >>= fun _ ->
            Lwt.return true
          end
        end
        else begin 
          Logs.info (fun m -> m "Not using a repo.");
          Lwt.return false
        end
    end 
end

