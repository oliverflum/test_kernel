open Lwt.Infix

exception UnsupportedType
exception WrongType of string
module StringMap = Map.Make(String)
module JS = Yojson.Basic

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

class webStore ctx resolver uuid = 
  let ctx = Cohttp_mirage.Client.ctx resolver ctx in
  object (self)
    val store_ctx = ctx
    val uuid = uuid
    val mutable map = StringMap.empty

    method private http_get (uri: Uri.t) =  
      Cohttp_mirage.Client.get ~ctx:store_ctx uri >>= fun (response, body) ->
      Cohttp_lwt.Body.to_string body

    method private http_post (uri: Uri.t) (body: string) =
      let body = Cohttp_lwt.Body.of_string body in
      let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
      Cohttp_mirage.Client.post ~ctx:store_ctx ~body ~headers uri >>= fun (response, body) ->
      Lwt.return response.status

    method private map_to_json =
      let js_map = StringMap.map (fun v -> (vtype_to_json v)) map in
      let l = List.of_seq (StringMap.to_seq js_map) in
      let json = `Assoc l in 
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

    method suspend = 
      OS.Xs.make () >>= fun client ->
      OS.Xs.(immediate client (fun h -> directory h "data")) >>= fun dir -> 
      if List.mem "target_url" dir then begin
        OS.Xs.(immediate client (fun h -> read h "data/target_url")) >>= fun target_uri ->
        OS.Xs.(immediate client (fun h -> read h "name")) >>= fun name ->
        let uri = Uri.of_string (target_uri^"/store?uuid="^uuid^"&name="^name) in
        let body = self#map_to_json in
        self#http_post uri body >>= fun _ ->
        OS.Sched.shutdown OS.Sched.Poweroff;
        Lwt.return ()
      end else begin
        OS.Sched.shutdown OS.Sched.Poweroff;
        Lwt.return ()
      end

    method init (origin_uri: string) =
      Logs.info (fun m -> m "origin_uri: %s" origin_uri);
      if origin_uri == "" then Lwt.return ()
      else begin 
        let uri = Uri.of_string (origin_uri^"/store?uuid="^uuid) in
        self#http_get uri >>= fun body_str ->
        Logs.info (fun m -> m "body_str: %s" body_str);
        if body_str <> "" then
          let json = JS.from_string body_str in 
          self#store_all json;
          Lwt.return ()
        else 
          Lwt.return ()
      end
  end 