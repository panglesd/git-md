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

  let ( let*+ ) a b =
    let open Lwt.Syntax in
    let* x = a in
    match x with
    | Error e -> Lwt.return (Error e)
    | Ok o ->
        let+ r = b o in
        Ok r

  let ( let+* ) a b =
    let open Lwt.Syntax in
    let+ x = a in
    let open Result_syntax in
    let= x = x in
    b x
end
