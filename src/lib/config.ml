let ( let+ ) a b = Result.map b a
let conf_file dir = Fpath.(dir / ".git_md")

type config = (Hockmd.V1.Types.note_id * Fpath.t) list

let get_config dir =
  let conf_file = conf_file dir in
  let+ conf_content = Bos.OS.File.read_lines conf_file in
  List.fold_left
    (fun acc s ->
      match Astring.String.cut ~sep:":" s with
      | None ->
          Logs.warn (fun m -> m "Ignoring unparsable line: %s" s);
          acc
      | Some (id, filename) -> (id, Fpath.v filename) :: acc)
    [] conf_content

let set_config dir config =
  let lines =
    List.map (fun (id, file) -> Format.asprintf "%s:%a" id Fpath.pp file) config
  in
  let conf_file = conf_file dir in
  Bos.OS.File.write_lines conf_file lines

let filename_of_id id config = List.assoc_opt id config
let add_files config files = config @ files
let to_list = Fun.id
