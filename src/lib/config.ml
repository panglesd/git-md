let ( let+ ) a b = Result.map b a
let ( let* ) b a = Result.bind b a
let conf_file dir = Fpath.(dir / ".git_md")
let config_file = Fpath.v ".git_md"

type config = (Hockmd.V1.Types.note_id * Fpath.t) list

let get_config dir =
  let conf_file = conf_file dir in
  let* () =
    let* exist = Bos.OS.File.exists conf_file in
    if not exist then Bos.OS.File.write_lines conf_file [] else Ok ()
  in
  let+ conf_content = Bos.OS.File.read_lines conf_file in
  List.filter_map
    (fun s ->
      match Astring.String.cut ~sep:":" s with
      | Some (id, filename) -> Some (id, Fpath.v filename)
      | None ->
          Logs.warn (fun m -> m "Ignoring unparsable line: %s" s);
          None)
    conf_content

let set_config dir config =
  let lines =
    List.map (fun (id, file) -> Format.asprintf "%s:%a" id Fpath.pp file) config
  in
  let conf_file = conf_file dir in
  Bos.OS.File.write_lines conf_file lines

let filename_of_id id config = List.assoc_opt id config

let id_of_filename file config =
  List.assoc_opt file @@ List.map (fun (x, y) -> (y, x)) config

let set_file config id file =
  List.map (function i, _ when String.equal id i -> (i, file) | a -> a) config

let update_config config notes =
  let old_config = config in

  List.fold_left
    (fun config (note : Hockmd.V1.Types.note_summary) ->
      let path =
        match List.assoc_opt note.id old_config with
        | None -> Fpath.v (note.id ^ ".md")
        | Some path -> path
      in
      (note.id, path) :: config)
    [] notes

let obsolete_files config notes =
  let new_config = update_config config notes in
  let module S = Set.Make (Fpath) in
  let old = S.of_list (List.map (fun (_, p) -> p) config)
  and new_ = S.of_list (List.map (fun (_, p) -> p) new_config) in
  let remove = S.diff old new_ in
  S.to_seq remove |> List.of_seq

let of_list = Fun.id
let to_list = Fun.id
let add entry config = entry :: config
