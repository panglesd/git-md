val upload_pack :
  string option -> Fpath.t -> (unit, [ `Msg of string ]) result Lwt.t

val receive_pack :
  string option -> Fpath.t -> (unit, [ `Msg of string ]) result Lwt.t
