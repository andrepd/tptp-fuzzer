open Range

type t = {
  num_clauses: int range;
  num_literals_per_clause: int range;

  (* num_vars_per_clause: int range; *)
  num_vars: int range;
  (* num_vars_total: int range; *)
  (* num_vars_occurences_per_clause: int range;
  num_vars_ocurrences_total: int range; *)
  ratio_vars: float;  (* Over total *)
  
  num_funcs: int range;
  funcs_arity: int range;
  
  num_preds: int range;
  preds_arity: int range;
  
  max_depth: int;
}
