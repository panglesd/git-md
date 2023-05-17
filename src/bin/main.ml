open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let token =
  let doc = "The token to use to connect." in
  Arg.(
    required & opt (some string) None & info ~docv:"TOKEN" ~doc [ "token"; "t" ])

let dir_opt =
  let doc = "Directory on which to store files." in
  Arg.(value & opt (some dir) None & info ~docv:"DIR" ~doc [ "directory"; "d" ])

let dir =
  let doc = "Directory on which to store files." in
  Arg.(value & pos 0 string "." & info ~docv:"DIR" ~doc [])

let api_opt =
  let doc = "The url of the API." in
  Arg.(value & opt (some string) None & info ~docv:"URL" ~doc [ "api"; "a" ])

let wrapper f token dir api () =
  let token = Hockmd.V1.token_of_string token in
  let dir = match dir with None -> Fpath.v "." | Some v -> Fpath.v v in
  match Lwt_main.run @@ f token dir api with
  | Ok () -> Ok ()
  | Error (`Msg s) -> Error s

let pull =
  let pull = wrapper Git_md.pull in
  Term.(const pull $ token $ dir_opt $ api_opt $ setup_log)

let pull_cmd =
  let doc = "Pull hackmd to local files" in
  let man = [] in
  let info = Cmd.info "pull" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info pull

let push =
  let push = wrapper Git_md.push in
  Term.(const push $ token $ dir_opt $ api_opt $ setup_log)

let push_cmd =
  let doc = "Push hackmd to local files" in
  let man = [] in
  let info = Cmd.info "push" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info push

let upload_pack =
  let upload_pack api dir () =
    let dir = Fpath.v dir in
    match Lwt_main.run @@ Git_md.upload_pack api dir with
    | Ok () -> Ok ()
    | Error (`Msg s) -> Error s
  in
  Term.(const upload_pack $ api_opt $ dir $ setup_log)

let upload_pack_cmd =
  let doc = "Upload-pack hackmd" in
  let man = [] in
  let info = Cmd.info "upload-pack" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info upload_pack

let receive_pack =
  let receive_pack api dir () =
    let dir = Fpath.v dir in
    match Lwt_main.run @@ Git_md.receive_pack api dir with
    | Ok () -> Ok ()
    | Error (`Msg s) -> Error s
  in
  Term.(const receive_pack $ api_opt $ dir $ setup_log)

let receive_pack_cmd =
  let doc = "Upload-pack hackmd" in
  let man = [] in
  let info = Cmd.info "receive-pack" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info receive_pack

let cmd =
  let doc = "Access hackmd to local files" in
  let man = [] in
  let info = Cmd.info "hockmd" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.group info [ pull_cmd; push_cmd; upload_pack_cmd; receive_pack_cmd ]

let main () = exit (Cmd.eval_result cmd)
let () = main ()
