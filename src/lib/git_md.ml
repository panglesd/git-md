module Hmd = Hockmd.V1

module Result_syntax = struct
  let ( let= ) = Result.bind
  let ( let| ) a b = Result.map b a
end

module Combined_syntax = struct
  let ( let++ ) a b =
    let open Lwt.Syntax in
    let+ x = a in
    let open Result_syntax in
    let| x = x in
    b x

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
  (* stat --format='%.3X' gnTW9iUwS2i8nv4p2UwiWQ.md *)
  let cmd = Bos.Cmd.(v "stat" % "--format" % "%.3X" % p f) in
  let| d = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string in
  Logs.warn (fun m -> m "Timestamp is %s" d);
  let ios = int_of_string in
  match String.split_on_char ',' d with
  | [ i; s ] -> (ios i * 1000) + ios s (* Hackmd uses "milliseconds epoch" *)
  | _ -> failwith ""

let set_last_changed f timestamp =
  (* Hackmd uses "milliseconds epoch" *)
  let s = string_of_int timestamp in
  let i = String.sub s 0 (String.length s - 3)
  and s = String.sub s (String.length s - 3) 3 in
  let timestamp = "@" ^ i ^ "," ^ s in
  let cmd = Bos.Cmd.(v "touch" % "-d" % timestamp % p f) in
  Bos.OS.Cmd.run cmd

let get_notes ?api_url token =
  let open Combined_syntax in
  Logs.warn (fun m ->
      m "Pulling notes from %s"
        (match api_url with None -> "default repo" | Some v -> v));
  let++ notes = Hmd.notes ?api_url token in
  Logs.warn (fun m ->
      m "Got notes %a"
        (Fmt.list ~sep:Fmt.cut Fmt.string)
        (List.map (fun (note : Hmd.Types.note_summary) -> note.id) notes));
  notes

let get_config dir =
  let open Combined_syntax in
  Logs.warn (fun m -> m "Reading config");
  (* Get config *)
  let++ config = Lwt.return @@ Config.get_config dir in
  Logs.warn (fun m -> m "Config read");
  config

let pull token dir api_url =
  let open Combined_syntax in
  (* Get notes *)
  let** notes = get_notes ?api_url token in
  (* Get config *)
  let** config = get_config dir in
  (* Update config given new note: add new notes to the config, and get obsolete
     ones. *)
  let to_remove = Config.obsolete_files config notes in
  (* Update a file-system note from a note in hackmd *)
  let write id file =
    Logs.warn (fun m -> m "Pulling note %s" id);
    (* Get the note *)
    let** note = Hmd.note ?api_url token id in
    Logs.debug (fun m -> m "Writing %a: %s" Fpath.pp file note.content);
    (* Write content *)
    let+* () =
      Lwt.return @@ Bos.OS.File.write (Fpath.( // ) dir file) note.content
    in
    (* Update timestamp *)
    set_last_changed (Fpath.( // ) dir file) note.lastChangedAt
  in
  (* Fold the notes and do the update if the timestamp says so. Update the
     config when creating new fles *)
  let+* config =
    Lwt_list.fold_left_s
      (fun r (note_summary : Hmd.Types.note_summary) ->
        let** config = Lwt.return r in
        let** file, file_timestamp, config =
          match Config.filename_of_id note_summary.id config with
          | None ->
              let file = Fpath.v (note_summary.id ^ ".md") in
              Lwt.return_ok (file, 0, Config.add (note_summary.id, file) config)
          | Some file ->
              let++ timestamp = Lwt.return @@ last_changed file in
              (file, timestamp, config)
        in
        let++ () =
          if file_timestamp < note_summary.lastChangedAt then
            write note_summary.id file
          else Lwt.return_ok ()
        in
        config)
      (Ok config) notes
  in
  (* Delete unnecessary files *)
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
  (* Get notes *)
  let** notes = get_notes ?api_url token in
  (* Get config *)
  let** config = get_config dir in
  (* Update function *)
  let update id path =
    Logs.warn (fun m -> m "Sending note %s" id);
    match Bos.OS.File.read (Fpath.( // ) dir path) with
    | Error (`Msg s) ->
        Logs.debug (fun m -> m "Error when reading %a: %s" Fpath.pp path s);
        Lwt.return_ok ()
    | Ok content -> (
        let* p =
          Hmd.update_note ?api_url token id
            (Some { content; readPermission = Owner })
        in
        match p with
        | Error (`Msg s) ->
            Logs.debug (fun m -> m "Error when updating %s: %s" id s);
            Lwt.return (Ok ())
        | Ok _ ->
            let+* note = Hmd.note ?api_url token id in
            set_last_changed path note.lastChangedAt)
  in
  (* For each note _in the config_, update it if needed.  *)
  let** () =
    Lwt_list.fold_left_s
      (fun r (id, path) ->
        let** () = Lwt.return r in
        let server_timestamp =
          match
            List.find_opt
              (fun (note_summary : Hmd.Types.note_summary) ->
                String.equal note_summary.id id)
              notes
          with
          | None -> 0
          | Some n -> n.lastChangedAt
        in
        let** file_timestamp = Lwt.return @@ last_changed path in
        if server_timestamp < file_timestamp then update id path
        else Lwt.return_ok ())
      (Ok ()) (Config.to_list config)
  in
  (* Find files outside of the config *)
  let new_files =
    match Bos.OS.Dir.contents ~rel:true dir with
    | Error (`Msg s) ->
        Logs.warn (fun m ->
            m "Error when listing files in %a: %s" Fpath.pp dir s);
        []
    | Ok files ->
        List.filter
          (fun file ->
            Option.is_none (Config.id_of_filename file config)
            && Fpath.has_ext "md" file)
          files
  in
  (* Create notes and add the files to the config *)
  let+* config =
    Lwt_list.fold_left_s
      (fun config file ->
        let** config = Lwt.return config in
        Logs.warn (fun m -> m "Creating new note from %a" Fpath.pp file);
        let** content =
          Lwt.return @@ Bos.OS.File.read (Fpath.( // ) dir file)
        in
        let** note =
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
        let++ () = Lwt.return @@ set_last_changed file note.lastChangedAt in
        Config.add (note.id, file) config)
      (Ok config) new_files
  in
  Config.set_config dir config

(*   (\* Now, check new files to add them to the config *\) *)
(* let config_list = Config.to_list config in *)
(* match Bos.OS.Dir.contents ~rel:true dir with *)
(* | Error (`Msg s) -> *)
(*     Logs.warn (fun m -> m "Error when listing files in %a: %s" Fpath.pp dir s); *)
(*     Lwt.return_ok () *)
(* | Ok files -> *)
(*     let new_files = *)
(*       List.filter *)
(*         (fun file -> *)
(*           List.for_all *)
(*             (fun (_, (p, _)) -> not (Fpath.equal file p)) *)
(*             config_list *)
(*           && Fpath.has_ext "md" file) *)
(*         files *)
(*     in *)

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
      (fun err (_, path) ->
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
      (fun err (_, path) ->
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
