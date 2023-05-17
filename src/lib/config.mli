type config

val conf_file : Fpath.t -> Fpath.t
val config_file : Fpath.t
val get_config : Fpath.t -> (config, [ `Msg of string ]) result
val set_config : Fpath.t -> config -> (unit, [ `Msg of string ]) result
val filename_of_id : Hockmd.V1.Types.note_id -> config -> Fpath.t option
val last_updated_of_id : Hockmd.V1.Types.note_id -> config -> int option
val of_list : (Hockmd.V1.Types.note_id * (Fpath.t * int)) list -> config
val to_list : config -> (Hockmd.V1.Types.note_id * (Fpath.t * int)) list
val set_last_updated : config -> Hockmd.V1.Types.note_id -> int -> config
val set_file : config -> Hockmd.V1.Types.note_id -> Fpath.t -> config
val update_config : config -> Hockmd.V1.Types.note_summary list -> config
val obsolete_files : config -> Hockmd.V1.Types.note_summary list -> Fpath.t list
