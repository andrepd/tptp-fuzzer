(** Subcommand (options) | None *)
type result =
  | Print of Options.t
  | Compare of Options.t * (string * External.solver) list
  | Hammer of Options.t * (string * External.solver) list
  | Save of Options.t * int * string
  | Load of string * (string * External.solver) list

(** Parses subcommand + options *)
val top_parser : unit -> result
