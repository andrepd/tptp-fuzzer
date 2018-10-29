(** Subcommand (options) | None *)
type result =
  | Print of Options.t
  | Compare of Options.t * (string * External.solver) list

(** Parses subcommand + options *)
val top_parser : unit -> result
