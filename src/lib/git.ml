open Utils

let git_add dir file =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  let cmd = Bos.Cmd.(v "git" % "add" % p file) in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_commit dir message =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  let cmd = Bos.Cmd.(v "git" % "commit" % "-m" % message) in
  let () =
    match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null with
    | Ok () | Error _ -> ()
  in
  Bos.OS.Dir.set_current current

let git_init bare dir =
  let bare = if bare then Bos.Cmd.(v "--bare") else Bos.Cmd.empty in
  let cmd = Bos.Cmd.(v "git" % "init" %% bare % p dir) in
  Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null

let git_clone repo dir =
  (* Specify upload-pack *)
  let cmd = Bos.Cmd.(v "git" % "clone" % p repo % p dir) in
  Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null

let git_push dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  (* Specify upload-pack *)
  let cmd = Bos.Cmd.(v "git" % "push") in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_pull dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  (* Specify upload-pack *)
  let cmd = Bos.Cmd.(v "git" % "pull") in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_fetch dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  (* Specify upload-pack *)
  let cmd = Bos.Cmd.(v "git" % "fetch") in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let bare_repo = Fpath.v "bare_repo"
let hm_repo = Fpath.v "hm_repo"

let init dir =
  let open Result_syntax in
  (* Create the repository if needed *)
  let= _was_created_top = Bos.OS.Dir.create dir in
  let bare_dir = Fpath.(dir // bare_repo) in
  let hm_dir = Fpath.(dir // hm_repo) in
  let= was_created_bare = Bos.OS.Dir.create bare_dir in
  let= was_created_hm = Bos.OS.Dir.create hm_dir in
  let= () = if was_created_bare then git_init true bare_dir else Ok () in
  let= () = if was_created_hm then git_clone bare_dir hm_dir else Ok () in
  Ok (bare_dir, hm_dir)

let git_upload_pack dir =
  let cmd = Bos.Cmd.(v "git" % "upload-pack" % p dir) in
  Bos.OS.Cmd.run cmd

let git_receive_pack dir =
  let cmd = Bos.Cmd.(v "git" % "receive-pack" % p dir) in
  Bos.OS.Cmd.run cmd

let git_add_config dir =
  let config_file = Config.config_file in
  git_add dir config_file

let git_update token api_url dir =
  let open Combined_syntax in
  let open Result_syntax in
  (* Create and initialize, if needed, the bare and hm folder *)
  let** bare_dir, hm_dir = Lwt.return @@ init dir in
  (* Pull in the hm repo *)
  let+* () = Sync.pull token hm_dir api_url in
  (* Commit all files in the hm repo *)
  let= config = Config.get_config hm_dir in
  let= () =
    List.fold_left
      (fun err (_, path) ->
        let= () = err in
        git_add hm_dir path)
      (Ok ()) (Config.to_list config)
  in
  let= () = git_add_config hm_dir in
  let= () = git_commit hm_dir "Update from hackmd" in
  (* Push all this to the bare repo *)
  let= () = git_push hm_dir in
  Ok (bare_dir, hm_dir)

let upload_pack api_url dir =
  let open Combined_syntax in
  (* Extract token from directory *)
  let token = Hmd.token_of_string @@ Fpath.basename dir in
  Logs.warn (fun m -> m "Token is %s" (Fpath.basename dir));
  (* Update the bare repo *)
  let** bare_dir, _hm_dir = git_update token api_url dir in
  (* Answer the original request *)
  let++ res = Lwt.return @@ git_upload_pack bare_dir in
  Logs.warn (fun m -> m "Done");
  res

let files_to_update dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  (* Specify upload-pack *)
  let cmd =
    Bos.Cmd.(v "git" % "diff" % "--name-only" % "master..origin/master")
  in
  let= modified_files = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_lines in
  let| () = Bos.OS.Dir.set_current current in
  List.filter_map
    (function ".git_md" -> None | x -> Some (Fpath.v x))
    modified_files

let receive_pack api_url dir =
  let open Combined_syntax in
  (* Extract token from directory *)
  let token = Hmd.token_of_string @@ Fpath.basename dir in
  (* Update the bare repo *)
  let** bare_dir, hm_dir = git_update token api_url dir in
  (* Answer the original request *)
  let** () = Lwt.return @@ git_receive_pack bare_dir in

  let** () = Lwt.return @@ git_fetch hm_dir in

  let** modified_files = Lwt.return @@ files_to_update hm_dir in

  (* Pull from bare to hm *)
  let** () = Lwt.return @@ git_pull hm_dir in
  (* Push changes to hackmd *)
  let** () = Lwt.return @@ git_add_config hm_dir in
  let** () = Lwt.return @@ git_commit hm_dir "Update from hackmd" in
  (* Push all this to the bare repo *)
  let** () = Lwt.return @@ git_push hm_dir in
  let++ res = Sync.push_files token hm_dir modified_files api_url in
  Logs.warn (fun m -> m "Done");
  res
