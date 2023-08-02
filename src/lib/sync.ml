open Utils

let last_changed dir f =
  let f = Fpath.( // ) dir f in
  let open Result_syntax in
  (* stat --format='%.3X' gnTW9iUwS2i8nv4p2UwiWQ.md *)
  let cmd = Bos.Cmd.(v "stat" % "--format" % "%.3X" % p f) in
  let| d = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string in
  let ios = int_of_string in
  match String.split_on_char ',' d with
  | [ i; s ] -> (ios i * 1000) + ios s (* Hackmd uses "milliseconds epoch" *)
  | _ -> (
      (* some version use '.' *)
      match String.split_on_char '.' d with
      | [ i; s ] ->
          (ios i * 1000) + ios s (* Hackmd uses "milliseconds epoch" *)
      | _ -> failwith ("do not recognize last changed " ^ d))

let set_last_changed dir f timestamp =
  (* Hackmd uses "milliseconds epoch" *)
  let f = Fpath.( // ) dir f in
  let s = string_of_int timestamp in
  let i = String.sub s 0 (String.length s - 3)
  and s = String.sub s (String.length s - 3) 3 in
  let timestamp = "@" ^ i ^ "," ^ s in
  let cmd = Bos.Cmd.(v "touch" % "-d" % timestamp % p f) in
  Bos.OS.Cmd.run cmd

let get_notes ?api_url token =
  let open Combined_syntax in
  Logs.warn (fun m ->
      m "Pulling notes from %s..."
        (match api_url with None -> "default repo" | Some v -> v));
  let++ notes = Hmd.notes ?api_url token in
  Logs.warn (fun m ->
      m "Found %d notes on the repository" (List.length notes)
      (* (Fmt.list ~sep:Fmt.sp Fmt.string) *)
      (* (List.map (fun (note : Hmd.Types.note_summary) -> note.id) notes) *));
  notes

let pull token dir api_url =
  let open Combined_syntax in
  (* Get notes *)
  let** notes = get_notes ?api_url token in
  (* Get config *)
  let** config = Lwt.return @@ Config.get_config dir in
  (* Update config given new note: add new notes to the config, and get obsolete
     ones. *)
  let to_remove = Config.obsolete_files config notes in
  (* Update a file-system note from a note in hackmd *)
  let write id title file =
    Logs.warn (fun m -> m "Pulling note \"%s\" (%s)..." title id);
    (* Get the note *)
    let** note = Hmd.note ?api_url token id in
    Logs.debug (fun m -> m "Writing %a: %s" Fpath.pp file note.content);
    (* Write content *)
    let+* () =
      Lwt.return @@ Bos.OS.File.write (Fpath.( // ) dir file) note.content
    in
    (* Update timestamp *)
    set_last_changed dir file note.lastChangedAt
  in
  (* Fold the notes and accumulate note+file if the timestamp says so. Update the
     config when creating new files *)
  let** config, files_to_update =
    let open Result_syntax in
    let res =
      List.fold_left
        (fun r (note_summary : Hmd.Types.note_summary) ->
          let= config, notes_with_file = r in
          let= file, file_timestamp, config =
            match Config.filename_of_id note_summary.id config with
            | None ->
                let file = Fpath.v (note_summary.id ^ ".md") in
                let config = Config.add (note_summary.id, file) config in
                Ok (file, 0, config)
            | Some file ->
                let| timestamp = last_changed dir file in
                (file, timestamp, config)
          in
          let lastChangedAt =
            match note_summary.lastChangedAt with
            | None -> Int.max_int
            | Some n -> n
          in
          if file_timestamp < lastChangedAt then
            Ok (config, (note_summary, file) :: notes_with_file)
          else Ok (config, notes_with_file))
        (Ok (config, []))
        notes
    in
    Lwt.return res
  in
  Logs.warn (fun m ->
      m "Found %d notes needing update: %a..."
        (List.length files_to_update)
        (Fmt.list ~sep:Fmt.sp Fmt.string)
        (List.map
           (fun ((note : Hmd.Types.note_summary), _) -> note.id)
           files_to_update));
  (* Do the update for aggregated notes+file *)
  let** () =
    Lwt_list.fold_left_s
      (fun r ((note_summary : Hmd.Types.note_summary), file) ->
        let** () = Lwt.return r in
        write note_summary.id note_summary.title file)
      (Ok ()) files_to_update
  in
  let other_files_to_update = Config.other_files config in
  let+* () =
    Lwt_list.fold_left_s
      (fun r (id, file) ->
        let** () = Lwt.return r in
        write id "Not owned note" file)
      (Ok ()) other_files_to_update
  in
  (* Delete unnecessary files *)
  List.iter
    (fun path ->
      Logs.warn (fun m ->
          m "Deleting %a as it's not anymore in Hackmd..." Fpath.pp path);
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
  let** config = Lwt.return @@ Config.get_config dir in
  (* Update function *)
  let update id title path =
    Logs.warn (fun m -> m "Sending note \"%s\" (%s)" title id);
    match Bos.OS.File.read (Fpath.( // ) dir path) with
    | Error (`Msg s) ->
        Logs.debug (fun m -> m "Error when reading %a: %s" Fpath.pp path s);
        Lwt.return_ok ()
    | Ok content -> (
        let* p =
          Hmd.update_note ?api_url token id
            (Some { content; readPermission = None })
        in
        match p with
        | Error (`Msg s) ->
            Logs.debug (fun m -> m "Error when updating %s: %s" id s);
            Lwt.return (Ok ())
        | Ok _ ->
            let+* note = Hmd.note ?api_url token id in
            set_last_changed dir path note.lastChangedAt)
  in
  (* For each note _in the config_, update it if needed.  *)
  let** files_to_update =
    let res =
      List.fold_left
        (fun r (id, path) ->
          let open Result_syntax in
          let= r = r in
          let server_timestamp, title =
            match
              List.find_opt
                (fun (note_summary : Hmd.Types.note_summary) ->
                  String.equal note_summary.id id)
                notes
            with
            | None -> (0, "Untitled" (* has been deleted... *))
            | Some n ->
                let last = Option.value n.lastChangedAt ~default:0 in
                (last, n.title)
          in
          let| file_timestamp = last_changed dir path in
          if server_timestamp < file_timestamp then (id, title, path) :: r
          else r)
        (Ok []) (Config.to_list config)
    in
    Lwt.return res
  in
  Logs.warn (fun m ->
      m "Found %d notes needing update: %a..."
        (List.length files_to_update)
        (Fmt.list ~sep:Fmt.sp Fmt.string)
        (List.map (fun (id, _, _) -> id) files_to_update));
  let** () =
    Lwt_list.fold_left_s
      (fun r (id, title, path) ->
        let** () = Lwt.return r in
        update id title path)
      (Ok ()) files_to_update
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
        let++ () = Lwt.return @@ set_last_changed dir file note.lastChangedAt in
        Config.add (note.id, file) config)
      (Ok config) new_files
  in
  Config.set_config dir config

let push_files token dir files api_url =
  let open Combined_syntax in
  (* Get config *)
  let** config = Lwt.return @@ Config.get_config dir in
  (* Update function *)
  let update id path =
    Logs.warn (fun m -> m "Sending note %a to hackmd note %s" Fpath.pp path id);
    match Bos.OS.File.read (Fpath.( // ) dir path) with
    | Error (`Msg s) ->
        Logs.debug (fun m -> m "Error when reading %a: %s" Fpath.pp path s);
        Lwt.return (Ok config)
    | Ok content ->
        let+* _p =
          Hmd.update_note ?api_url token id
            (Some { content; readPermission = None })
        in
        Ok config
  in
  let create config file =
    Logs.warn (fun m -> m "Creating new note from %a" Fpath.pp file);
    let** content = Lwt.return @@ Bos.OS.File.read (Fpath.( // ) dir file) in
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
    let++ () = Lwt.return @@ set_last_changed dir file note.lastChangedAt in
    Config.add (note.id, file) config
  in
  (* For each file to update  *)
  let+* config =
    Lwt_list.fold_left_s
      (fun config filename ->
        let** config = Lwt.return config in
        match Config.id_of_filename filename config with
        | None -> create config filename
        | Some id -> update id filename)
      (Ok config) files
  in
  Config.set_config dir config
