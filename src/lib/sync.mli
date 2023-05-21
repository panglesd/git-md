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
