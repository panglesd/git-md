let ( let+ ) a b = Result.map b a
let ( let* ) b a = Result.bind b a
let conf_file dir = Fpath.(dir / ".git_md")
let config_file = Fpath.v ".git_md"

type path = Fpath.t
type note_id = Hockmd.V1.Types.note_id

let note_id_of_yojson x =
  match x with `String x -> x | _ -> failwith "error when reading note id"

let yojson_of_note_id x = `String x

let path_of_yojson x =
  match x with
  | `String x -> (
      match Fpath.of_string x with Ok x -> x | Error _ -> failwith "")
  | _ -> failwith "error when reading note id"

let yojson_of_path x = `String (Fpath.to_string x)

type note = { id : note_id; path : path } [@@deriving yojson]

type config = { owned_notes : note list; other_notes : note list }
[@@deriving yojson]

let empty_config = { owned_notes = []; other_notes = [] }

let write_config_file conf_file config =
  Bos.OS.File.with_oc conf_file
    (fun oc () ->
      Ok (Yojson.Safe.pretty_to_channel oc (yojson_of_config config)))
    ()
  |> Result.join

let read_config_file conf_file =
  config_of_yojson @@ Yojson.Safe.from_file (Fpath.to_string conf_file)

let get_config dir =
  Logs.warn (fun m -> m "Reading config...");
  let conf_file = conf_file dir in
  let* () =
    let* exist = Bos.OS.File.exists conf_file in
    if not exist then write_config_file conf_file empty_config else Ok ()
  in
  let res = read_config_file conf_file in
  Logs.warn (fun m -> m "Config read");
  Ok res

let set_config dir config =
  Logs.warn (fun m -> m "Writing new config...");
  let conf_file = conf_file dir in
  let+ () = write_config_file conf_file config in
  Logs.warn (fun m -> m "New config written.")

let filename_of_id id_key config =
  let find_id =
    List.find_map (fun { id; path } -> if id = id_key then Some path else None)
  in
  match find_id config.owned_notes with
  | Some p -> Some p
  | None -> find_id config.other_notes

let id_of_filename file config =
  let find_path =
    List.find_map (fun { id; path } ->
        if Fpath.equal path file then Some id else None)
  in
  match find_path config.owned_notes with
  | Some p -> Some p
  | None -> find_path config.other_notes

(* let set_file config id file = *)
(*   List.map (function i, _ when String.equal id i -> (i, file) | a -> a) config *)

let update_config (config : config) notes =
  let old_owned_notes = config.owned_notes in
  let owned_notes =
    List.fold_left
      (fun owned_notes (note : Hockmd.V1.Types.note_summary) ->
        let note =
          match List.find_opt (fun n -> n.id = note.id) old_owned_notes with
          | None -> { id = note.id; path = Fpath.v (note.id ^ ".md") }
          | Some note -> note
        in
        note :: owned_notes)
      [] notes
  in
  { config with owned_notes }

let obsolete_files config notes =
  let new_config = update_config config notes in
  let module S = Set.Make (Fpath) in
  let old = S.of_list (List.map (fun { path; _ } -> path) config.owned_notes)
  and new_ =
    S.of_list (List.map (fun { path; _ } -> path) new_config.owned_notes)
  in
  let remove = S.diff old new_ in
  S.to_seq remove |> List.of_seq

(* let of_list = List.map (fun (id, path) -> { id; path }) *)
let to_list c =
  let f = List.rev_map (fun { id; path } -> (id, path)) in
  List.rev_append (f c.owned_notes) (f c.other_notes)

let add (id, path) config =
  { config with owned_notes = { id; path } :: config.owned_notes }

let other_files config =
  List.rev_map (fun { id; path } -> (id, path)) config.other_notes
