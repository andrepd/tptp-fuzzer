(** Solver result type *)
type result = 
  | Sat
  | Unsat
  | Unknown

val result_to_string : result -> string

(** Represents a solver. Args: problem in TPTP format. Returns: Sat/Unsat or Unknown *)
type solver = string -> result

(** Returns a solver given a path to iprover binary *)
val iprover : string -> solver

(** Returns a solver given a path to vampire binary *)
val vampire : string -> solver
