open Cmdliner

let git_shell =
  let git_shell c =
    match c with
    | None -> failwith "unacceptable non command"
    | Some s -> (
        match String.split_on_char ' ' s with
        | [ "git-upload-pack"; dir ] -> (
            let dir = String.sub dir 1 (String.length dir - 2) in
            let dir = Fpath.v dir in
            match Lwt_main.run @@ Git_md.Git.upload_pack None dir with
            | Ok () -> Ok ()
            | Error (`Msg s) -> Error s)
        | [ "git-receive-pack"; dir ] -> (
            let dir = String.sub dir 1 (String.length dir - 2) in
            let dir = Fpath.v dir in
            match Lwt_main.run @@ Git_md.Git.receive_pack None dir with
            | Ok () -> Ok ()
            | Error (`Msg s) -> Error s)
        | _ -> failwith ("unacceptable command: " ^ s))
  in
  let c =
    let doc = "Execute a blessed command." in
    Arg.(value & opt (some string) None & info ~docv:"CMD" ~doc [ "c" ])
  in
  (* let dir = *)
  (*   let doc = "Directory on which to store files." in *)
  (*   Arg.(value & pos 0 string "." & info ~docv:"DIR" ~doc []) *)
  (* in *)
  Term.(const git_shell $ c (* $ dir *))

let git_shell_cmd =
  let doc = "git_shell hackmd" in
  let man = [] in
  let info = Cmd.info "git-shell" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info git_shell

let main () = exit (Cmd.eval_result git_shell_cmd)
let () = main ()
