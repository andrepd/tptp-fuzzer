(** Subcommand (options) | None *)
type result =
  | Print of Options.t
  | Incremental of Options.t
  | None

(** Parses subcommand + options *)
val top_parser : unit -> result
