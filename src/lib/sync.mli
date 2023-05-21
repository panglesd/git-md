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

val push_files :
  Hockmd.V1.token ->
  Fpath.t ->
  Fpath.t list ->
  string option ->
  (unit, [ `Msg of string ]) result Lwt.t
