type config

val get_config : Fpath.t -> (config, [ `Msg of string ]) result
val set_config : Fpath.t -> config -> (unit, [ `Msg of string ]) result
val filename_of_id : Hockmd.V1.Types.note_id -> config -> Fpath.t option
val add_files : config -> (Hockmd.V1.Types.note_id * Fpath.t) list -> config
val to_list : config -> (Hockmd.V1.Types.note_id * Fpath.t) list
