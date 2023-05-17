let ( let+ ) a b = Result.map b a
let ( let* ) b a = Result.bind b a
let conf_file dir = Fpath.(dir / ".git_md")
let config_file = Fpath.v ".git_md"

type config = (Hockmd.V1.Types.note_id * (Fpath.t * int)) list

let get_config dir =
  let conf_file = conf_file dir in
  let* () =
    let* exist = Bos.OS.File.exists conf_file in
    if not exist then Bos.OS.File.write_lines conf_file [] else Ok ()
  in
  let+ conf_content = Bos.OS.File.read_lines conf_file in
  List.fold_left
    (fun acc s ->
      match Astring.String.cuts ~sep:":" s with
      | [ id; filename; lastChangedAt ] ->
          (id, (Fpath.v filename, int_of_string lastChangedAt)) :: acc
      | _ ->
          Logs.warn (fun m -> m "Ignoring unparsable line: %s" s);
          acc)
    [] conf_content

let set_config dir config =
  let lines =
    List.map
      (fun (id, (file, lCA)) -> Format.asprintf "%s:%a:%d" id Fpath.pp file lCA)
      config
  in
  let conf_file = conf_file dir in
  Bos.OS.File.write_lines conf_file lines

let filename_of_id id config = Option.map fst @@ List.assoc_opt id config
let last_updated_of_id id config = Option.map snd @@ List.assoc_opt id config

let set_file config id file =
  List.map
    (function i, (_, la) when String.equal id i -> (i, (file, la)) | a -> a)
    config

let set_last_updated config id la =
  List.map
    (function i, (file, _) when String.equal id i -> (i, (file, la)) | a -> a)
    config

let update_config config notes =
  let old_config = config in

  List.fold_left
    (fun config (note : Hockmd.V1.Types.note_summary) ->
      let path =
        match List.assoc_opt note.id old_config with
        | None -> Fpath.v (note.id ^ ".md")
        | Some (path, _) -> path
      in
      (note.id, (path, note.lastChangedAt)) :: config)
    [] notes

let obsolete_files config notes =
  let new_config = update_config config notes in
  let module S = Set.Make (Fpath) in
  let old = S.of_list (List.map (fun (_, (p, _)) -> p) config)
  and new_ = S.of_list (List.map (fun (_, (p, _)) -> p) new_config) in
  let remove = S.diff old new_ in
  S.to_seq remove |> List.of_seq

let of_list = Fun.id
let to_list = Fun.id
