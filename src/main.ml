open Batteries

open Types
open Range

let random_in (range: int range) =
  let diff = (range.upper) - (range.lower) + 1 in
  let n = Random.int diff in
  range.lower + n



let generate_term (options: Options.t) ~funcs ~num_vars : string term =
  let rec aux depth = 
    (* We have to pick a variable or a constant so we don't recurse *)
    if depth = 0 then (
      let constants = List.filter (fun (_,arity) -> arity = 0) funcs in
      (* If there are no constants, we have to choose a variable *)
      if List.is_empty constants then (
        let i = Random.int num_vars + 1 in
        Var ("X" ^ Int.to_string i)        
      ) 
      (* Otherwise we can choose a variable or a constant *)
      else (
        let roll = Random.float 1. in
        if roll < options.ratio_vars then (
          let i = Random.int num_vars + 1 in
          Var ("X" ^ Int.to_string i)
        ) else (
          let name, _ = Random.choice (List.enum constants) in
          Func (name, [])
        )
      )
    )
    (* If we haven't hit the bottom we can pick anything *)
    else (
      let roll = Random.float 1. in
      if roll < options.ratio_vars then (
        let i = Random.int num_vars + 1 in
        Var ("X" ^ Int.to_string i)
      ) else (
        let name, arity = Random.choice (List.enum funcs) in
        Func (name, List.init arity (fun _ -> aux (pred depth)))
      )
    )
  in
  aux options.max_depth

let generate_atom (options: Options.t) ~funcs ~preds ~num_vars : string atom =
  let name, arity = Random.choice (List.enum preds) in
  Pred (name, List.init arity (fun _ -> generate_term options ~funcs ~num_vars))

let generate_literal (options: Options.t) ~funcs ~preds ~num_vars : string literal =
  {
    sign = Random.bool();
    atom = generate_atom options ~funcs ~preds ~num_vars;
  }

let generate_clause (options: Options.t) ~funcs ~preds ~num_vars : string clause =
  let size = random_in options.num_literals_per_clause in
  List.init size (fun _ ->
    generate_literal options ~funcs ~preds ~num_vars (* options.max_depth *)
  )

(* let generate_clauseset *)

let generate_clauseset_incremental (options: Options.t) : string clauseset Enum.t =
  let list_funcs : (string * int) list = 
    let num_funcs = random_in options.num_funcs in
    List.init num_funcs (fun i ->
      let arity = random_in options.funcs_arity in
      let name = (if arity = 0 then "c" else "f") ^ Int.to_string i in
      (name, arity)
    )
  in
  let list_preds : (string * int) list = 
    let num_preds = random_in options.num_preds in
    List.init num_preds (fun i ->
      let arity = random_in options.preds_arity in
      let name = ("p") ^ Int.to_string i in
      (name, arity)
    )
  in
  let num_clauses = random_in options.num_clauses in
  let num_vars = random_in options.num_vars in
  
  let set = ref [] in

  let loop() =
    let clause = generate_clause options ~funcs:list_funcs ~preds:list_preds ~num_vars in
    set := !set @ [clause];
    !set
  in
  Enum.init num_clauses (fun _ -> loop())



let opt_parser() : Options.t = 
  let open BatOptParse.StdOpt in
  let open BatOptParse.OptParser in

  let num_clauses             = str_option() in
  let num_literals_per_clause = str_option() in
  let num_vars                = str_option() in
  let ratio_vars              = float_option() in
  let num_funcs               = str_option() in
  let funcs_arity             = str_option() in
  let num_preds               = str_option() in
  let preds_arity             = str_option() in
  let max_depth               = int_option() in

  let parse_range (r: string) : int range =
    (* prerr_endline r; *)
    try
      Scanf.sscanf r "%d--%d" (--)
    with (* Scanf.Scan_failure _ | *) End_of_file ->
      Scanf.sscanf r "%d" (fun x -> x--x)
  in

  let parser = make
    ~usage:"%prog options"
    ~version:"0.1"
    ()
  in
  
  add parser num_clauses             ~long_name:"num-clauses"             ~help:"Number of clauses (number or range)";
  add parser num_literals_per_clause ~long_name:"num-literals-per-clause" ~help:"Number of literals per clause (number or range)";
  add parser num_vars                ~long_name:"num-vars"                ~help:"Number of total variables (number or range)";
  add parser ratio_vars              ~long_name:"ratio-vars"              ~help:"Ratio of variables to all terms (float between 0 and 1)";
  add parser num_funcs               ~long_name:"num-funcs"               ~help:"Number of functions (number or range";
  add parser funcs_arity             ~long_name:"funcs-arity"             ~help:"Function arity (number or range)";
  add parser num_preds               ~long_name:"num-preds"               ~help:"Number of predicates (number or range)";
  add parser preds_arity             ~long_name:"preds-arity"             ~help:"Predicate arity (number or range)";
  add parser max_depth               ~long_name:"max-depth"               ~help:"Maximum term depth (number)";

  ignore @@ parse_argv parser;

  let get_opt = BatOptParse.Opt.get in

  try
    {
      num_clauses             = get_opt num_clauses             |> parse_range;
      num_literals_per_clause = get_opt num_literals_per_clause |> parse_range;
      num_vars                = get_opt num_vars                |> parse_range;
      ratio_vars              = get_opt ratio_vars;
      num_funcs               = get_opt num_funcs               |> parse_range;
      funcs_arity             = get_opt funcs_arity             |> parse_range;
      num_preds               = get_opt num_preds               |> parse_range;
      preds_arity             = get_opt preds_arity             |> parse_range;
      max_depth               = get_opt max_depth;
    }
  with BatOptParse.Opt.No_value -> (
    (* prerr_endline "All options are mandatory. See -h for help."; *)
    error parser ~status:1 "All options are mandatory. See -h for help.";
    exit 1
  )



let test() =
  let open Options in

  Random.init (Unix.gettimeofday() |> Int.of_float);

  (* let options = {
    num_clauses = 5--8;
    num_literals_per_clause = 1--4;

    num_vars = 3--3;
    ratio_vars = 0.315;
    
    num_funcs = 3--3;
    funcs_arity = 0--3;
    
    num_preds = 3--3;
    preds_arity = 1--3;
    
    max_depth = 3;
  }
  in *)

  let options = opt_parser() in

  let gen = generate_clauseset_incremental options |> List.of_enum in

  List.print ~first:"" ~last:"\n" ~sep:"\n\n---\n\n" (Clauseset.print_tptp String.print) stdout gen;
  
  if not (List.for_all (Clauseset.validate_tptp String.print) gen) then
    failwith "invalid tptp";

  ()

let main() =
  (* opt_parser() *)
  ()

let () = test()
