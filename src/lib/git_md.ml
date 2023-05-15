module Hmd = Hockmd.V1

module Result_syntax = struct
  let ( let= ) = Result.bind
  (* let ( let| ) a b = Result.map b a *)
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

  (* let ( let+* ) a b = *)
  (*   let open Lwt.Syntax in *)
  (*   let+ x = a in *)
  (*   let open Result_syntax in *)
  (*   let= x = x in *)
  (*   b x *)
end

let pull token dir api_url =
  let open Lwt.Syntax in
  let open Result_syntax in
  let open Combined_syntax in
  Logs.app (fun m ->
      m "Pulling notes from url %s"
        (match api_url with None -> "default repo" | Some v -> v));
  let** notes = Hmd.notes ?api_url token in
  Logs.app (fun m ->
      m "Got notes %a"
        (Fmt.list ~sep:Fmt.cut Fmt.string)
        (List.map (fun (note : Hmd.Types.note_summary) -> note.id) notes));
  let+ notes =
    Lwt_list.filter_map_p
      (fun (note_summary : Hmd.Types.note_summary) ->
        let+ res = Hmd.note ?api_url token note_summary.id in
        match res with Ok a -> Some a | Error _ -> None)
      notes
  in
  let= config = Config.get_config dir in
  let new_files =
    List.fold_left
      (fun new_files note ->
        let file, new_ =
          let id = Obj.magic note.Hmd.Types.id in
          match Config.filename_of_id id config with
          | None ->
              let file = Fpath.v (id ^ ".md") in
              (file, (id, file) :: new_files)
          | Some f -> (f, new_files)
        in
        let ct = note.Hmd.Types.content in
        Logs.warn (fun m -> m "Writing %a: %s" Fpath.pp file ct);
        Bos.OS.File.write (Fpath.( // ) dir file) ct
        |> Result.iter_error (fun (`Msg err) ->
               Logs.warn (fun m ->
                   m "Error when writing %a: %s" Fpath.pp file err));
        new_)
      [] notes
  in
  let () =
    let files_to_remove =
      List.filter
        (fun (id, _) ->
          List.exists (fun note -> String.equal id note.Hmd.Types.id) notes)
        (Config.to_list config)
    in
    List.iter
      (fun (_, path) ->
        match Bos.OS.File.delete path with
        | Error (`Msg s) ->
            Logs.warn (fun m -> m "Error when deleting %a: %s" Fpath.pp path s)
        | Ok () -> ())
      files_to_remove
  in
  let new_config = Config.add_files config new_files in
  Config.set_config dir new_config

let push token dir api_url =
  let open Lwt.Syntax in
  match Config.get_config dir with
  | Error (`Msg s) ->
      Logs.warn (fun m -> m "Error when reading config gile: %s" s);
      Lwt.return_ok ()
  | Ok config -> (
      let* () =
        Lwt_list.iter_p
          (fun (id, path) ->
            match Bos.OS.File.read (Fpath.( // ) dir path) with
            | Error (`Msg s) ->
                Logs.warn (fun m ->
                    m "Error when reading %a: %s" Fpath.pp path s);
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
      let config_list = Config.to_list config in
      match Bos.OS.Dir.contents dir with
      | Error (`Msg s) ->
          Logs.warn (fun m ->
              m "Error when listing files in %a: %s" Fpath.pp dir s);
          Lwt.return_ok ()
      | Ok files ->
          let new_files =
            List.filter
              (fun file ->
                List.for_all
                  (fun (_, p) -> not (Fpath.equal file p))
                  config_list)
              files
          in
          let+ new_files =
            Lwt_list.filter_map_p
              (fun file ->
                match Bos.OS.File.read file with
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
                    | Ok note -> Some (note.id, file)
                    | Error (`Msg s) ->
                        Logs.warn (fun m ->
                            m "Error when creating note %a: %s" Fpath.pp file s);
                        None))
              new_files
          in
          let new_config = Config.add_files config new_files in
          Config.set_config dir new_config)

let git_upload_pack token dir api_url = () (* _ *)
