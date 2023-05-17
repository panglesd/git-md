val pull :
  Hockmd.V1.token ->
  Fpath.t ->
  string option ->
  (unit, [ `Msg of string ]) result Lwt.t

val push :
  Hockmd.V1.token ->
  Fpath.t ->
  string option ->
  (unit, [ `Msg of string ]) result Lwt.t

val upload_pack :
  string option -> Fpath.t -> (unit, [ `Msg of string ]) result Lwt.t

val receive_pack :
  string option -> Fpath.t -> (unit, [ `Msg of string ]) result Lwt.t
