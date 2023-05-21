module Hmd = Hockmd.V1

module Result_syntax = struct
  let ( let= ) = Result.bind
  let ( let| ) a b = Result.map b a
end

module Combined_syntax = struct
  (* let ( let++ ) a b = *)
  (*   let open Lwt.Syntax in *)
  (*   let+ x = a in *)
  (*   let open Result_syntax in *)
  (*   let| x = x in *)
  (*   b x *)

  let ( let** ) a b =
    let open Lwt.Syntax in
    let* x = a in
    match x with Error e -> Lwt.return (Error e) | Ok o -> b o

  (* let ( let*+ ) a b = *)
  (*   let open Lwt.Syntax in *)
  (*   let* x = a in *)
  (*   match x with *)
  (*   | Error e -> Lwt.return (Error e) *)
  (*   | Ok o -> *)
  (*       let+ r = b o in *)
  (*       Ok r *)

  let ( let+* ) a b =
    let open Lwt.Syntax in
    let+ x = a in
    let open Result_syntax in
    let= x = x in
    b x
end

let last_changed f =
  let open Result_syntax in
  let cmd = Bos.Cmd.(v "date" % "-r" % p f) in
  let| d = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string in
  int_of_string d

let pull token dir api_url =
  let open Combined_syntax in
  Logs.warn (fun m ->
      m "Pulling notes from %s"
        (match api_url with None -> "default repo" | Some v -> v));
  (* Get notes *)
  let** notes = Hmd.notes ?api_url token in
  Logs.warn (fun m ->
      m "Got notes %a"
        (Fmt.list ~sep:Fmt.cut Fmt.string)
        (List.map (fun (note : Hmd.Types.note_summary) -> note.id) notes));
  (* Get config *)
  let** config = Lwt.return @@ Config.get_config dir in
  (* Update config given new note *)
  let to_remove = Config.obsolete_files config notes
  and old_config = config
  and config = Config.update_config config notes in
  let write id =
    Logs.warn (fun m -> m "Pulling note %s" id);
    let+* note = Hmd.note ?api_url token id in
    let file =
      let id = note.Hmd.Types.id in
      match Config.filename_of_id id config with
      | None -> Fpath.v (id ^ ".md")
      | Some f -> f
    in
    let ct = note.Hmd.Types.content in
    Logs.debug (fun m -> m "Writing %a: %s" Fpath.pp file ct);
    Bos.OS.File.write (Fpath.( // ) dir file) ct
  in
  let+* () =
    Lwt_list.fold_left_s
      (fun r (note_summary : Hmd.Types.note_summary) ->
        let** () = Lwt.return r in
        match Config.last_updated_of_id note_summary.id old_config with
        | None -> write note_summary.id
        | Some last_updated when last_updated < note_summary.lastChangedAt ->
            write note_summary.id
        | _ -> Lwt.return_ok ())
      (Ok ()) notes
  in
  List.iter
    (fun path ->
      match Bos.OS.File.delete path with
      | Error (`Msg s) ->
          Logs.debug (fun m -> m "Error when deleting %a: %s" Fpath.pp path s)
      | Ok () -> ())
    to_remove;
  Config.set_config dir config

let push token dir api_url =
  let open Lwt.Syntax in
  let open Combined_syntax in
  Logs.warn (fun m -> m "Reading config");
  let** config = Lwt.return @@ Config.get_config dir in
  Logs.warn (fun m -> m "Config read");
  let* () =
    Lwt_list.iter_p
      (fun (id, (path, _)) ->
        Logs.warn (fun m -> m "Sending note %s" id);
        match Bos.OS.File.read (Fpath.( // ) dir path) with
        | Error (`Msg s) ->
            Logs.debug (fun m -> m "Error when reading %a: %s" Fpath.pp path s);
            Lwt.return ()
        | Ok content -> (
            let+ p =
              Hmd.update_note ?api_url token id
                (Some { content; readPermission = Owner })
            in
            match p with
            | Error (`Msg s) ->
                Logs.debug (fun m -> m "Error when updating %s: %s" id s)
            | Ok _ -> ()))
      (Config.to_list config)
  in
  (* Now, check new files to add them to the config *)
  let config_list = Config.to_list config in
  match Bos.OS.Dir.contents ~rel:true dir with
  | Error (`Msg s) ->
      Logs.warn (fun m -> m "Error when listing files in %a: %s" Fpath.pp dir s);
      Lwt.return_ok ()
  | Ok files ->
      let new_files =
        List.filter
          (fun file ->
            List.for_all
              (fun (_, (p, _)) -> not (Fpath.equal file p))
              config_list
            && Fpath.has_ext "md" file)
          files
      in
      let+ new_files =
        Lwt_list.filter_map_p
          (fun file ->
            Logs.warn (fun m -> m "Creating new note from %a" Fpath.pp file);
            match Bos.OS.File.read (Fpath.( // ) dir file) with
            | Error (`Msg s) ->
                Logs.warn (fun m ->
                    m "Error when reading %a: %s" Fpath.pp file s);
                Lwt.return None
            | Ok content -> (
                let+ res =
                  Hmd.create_note ?api_url token
                    (Some
                       {
                         title = Fpath.filename file;
                         content;
                         readPermission = Owner;
                         writePermission = Owner;
                         commentPermission = Owners;
                       })
                in
                match res with
                | Ok note -> Some (note.id, (file, note.lastChangedAt))
                | Error (`Msg s) ->
                    Logs.debug (fun m ->
                        m "Error when creating note %a: %s" Fpath.pp file s);
                    None))
          new_files
      in
      let new_config = Config.of_list (config_list @ new_files) in
      Config.set_config dir new_config

let git_add dir file =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  let cmd = Bos.Cmd.(v "git" % "add" % p file) in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_commit dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current dir in
  let cmd = Bos.Cmd.(v "git" % "commit" % "-m" % "Update from hackmd") in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_init dir =
  let cmd = Bos.Cmd.(v "git" % "init" % p dir) in
  Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null

let git_upload_pack dir =
  let cmd = Bos.Cmd.(v "git" % "upload-pack" % p dir) in
  Bos.OS.Cmd.run cmd

let git_receive_pack dir =
  let cmd =
    Bos.Cmd.(
      v "git" % "receive-pack" % p Fpath.(v "/" / "home" / "user" // dir))
  in
  Bos.OS.Cmd.run cmd

let git_hard_reset dir =
  let open Result_syntax in
  let= current = Bos.OS.Dir.current () in
  let= () = Bos.OS.Dir.set_current Fpath.(v "/" / "home" / "user" // dir) in
  let cmd = Bos.Cmd.(v "git" % "reset" % "--hard") in
  let= () = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_null in
  Bos.OS.Dir.set_current current

let git_add_config dir =
  let config_file = Config.config_file in
  git_add dir config_file

let upload_pack api_url dir =
  let open Combined_syntax in
  let open Result_syntax in
  let token = Hmd.token_of_string @@ Fpath.basename dir in
  let _ =
    match Bos.OS.Dir.create dir with
    | Error (`Msg _) -> failwith ""
    | Ok _ -> git_init dir
  in
  let+* () = pull token dir api_url in
  let= config = Config.get_config dir in
  let= () =
    List.fold_left
      (fun err (_, (path, _)) ->
        let= () = err in
        git_add dir path)
      (Ok ()) (Config.to_list config)
  in
  let= () = git_add_config dir in
  let= () = git_commit dir in
  git_upload_pack dir
(* _ *)

let receive_pack api_url dir =
  let open Combined_syntax in
  let open Result_syntax in
  let token = Hmd.token_of_string @@ Fpath.basename dir in
  let _ =
    match Bos.OS.Dir.create dir with
    | Error (`Msg _) -> failwith ""
    | Ok _ -> git_init dir
  in
  let** () = pull token dir api_url in
  let get_ok msg = function Error _ -> failwith msg | Ok x -> x in
  let config = Config.get_config dir |> get_ok "1" in
  let () =
    List.fold_left
      (fun err (_, (path, _)) ->
        let= () = err in
        git_add dir path)
      (Ok ()) (Config.to_list config)
    |> get_ok "2"
  in
  let () = git_commit dir |> function _ -> () in
  Logs.warn (fun m -> m "start receive_pack");
  let () =
    git_receive_pack dir |> function
    | Ok () -> ()
    | Error (`Msg s) -> Logs.warn (fun m -> m "error in receive_pack: %s" s)
  in
  Logs.warn (fun m -> m "end receive_pack");
  let () =
    git_hard_reset dir |> function
    | Ok () -> ()
    | Error (`Msg s) -> Logs.warn (fun m -> m "error in hard reset: %s" s)
  in
  Logs.warn (fun m -> m "Before the push");
  let dir = Fpath.(v "/" / "home" / "user" // dir) in
  let+* _ = push token dir api_url in
  (* let () = git_add_config dir |> get_ok "5" in *)
  (* let () = *)
  (*   git_commit dir |> function *)
  (*   | Ok () -> () *)
  (*   | Error (`Msg s) -> Logs.warn (fun m -> m "error in git commit: %s" s) *)
  (* in *)
  Ok ()
(* _ *)
