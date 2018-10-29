(** Subcommand (options) | None *)
type result =
  | Print of Options.t
  | Incremental of Options.t
  | Compare of Options.t * (string * External.solver) list
  | CompareIncremental of Options.t * (string * External.solver) list
  | None

(** Parses subcommand + options *)
val top_parser : unit -> result
