type config

val conf_file : Fpath.t -> Fpath.t
val config_file : Fpath.t
val get_config : Fpath.t -> (config, [ `Msg of string ]) result
val set_config : Fpath.t -> config -> (unit, [ `Msg of string ]) result
val filename_of_id : Hockmd.V1.Types.note_id -> config -> Fpath.t option
val id_of_filename : Fpath.t -> config -> Hockmd.V1.Types.note_id option
val of_list : (Hockmd.V1.Types.note_id * Fpath.t) list -> config
val to_list : config -> (Hockmd.V1.Types.note_id * Fpath.t) list
val set_file : config -> Hockmd.V1.Types.note_id -> Fpath.t -> config
val update_config : config -> Hockmd.V1.Types.note_summary list -> config
val obsolete_files : config -> Hockmd.V1.Types.note_summary list -> Fpath.t list
val add : Hockmd.V1.Types.note_id * Fpath.t -> config -> config
