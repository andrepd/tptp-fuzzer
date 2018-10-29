let version_number = "0.0.2"

open Batteries

open Range

type result =
  | Print of Options.t
  | Compare of Options.t * (string * External.solver) list

(* Helper functions *)
exception Bad_range
let parse_range (r: string) : int range =
  (* prerr_endline r; *)
  try
    Scanf.sscanf r "%d--%d" (--)
  with (* Scanf.Scan_failure _ | *) End_of_file -> try
    Scanf.sscanf r "%d" (fun x -> x--x)
  with (* Scanf.Scan_failure _ | *) End_of_file ->
    raise Bad_range

let range_option() : int range BatOptParse.Opt.t = 
  BatOptParse.Opt.value_option
    "<range>" (* metavar *)
    None (* default *)
    parse_range (* coerce *)
    (fun _ s -> Printf.sprintf "invalid range value '%s' (must be: <int>[--<int>])" s) (* errfmt *)

let int_option() =
  let open BatOptParse.StdOpt in
  int_option() ~metavar:"<int>"

let float_option() =
  let open BatOptParse.StdOpt in
  float_option() ~metavar:"<float>"

let formatter = 
  let terminal_width = 
    try
      Int.of_string (Sys.getenv "COLUMNS")
    with
    | Failure _ | Not_found -> 80
  in
  BatOptParse.Formatter.indented_formatter ~max_help_position:(terminal_width/2) ()

(* ----- *)

type options_parser_ret = {options: Options.t; args: string list; parser: BatOptParse.OptParser.t}
let options_parser() ~subcommand = 
  let open BatOptParse in
  let open BatOptParse.OptParser in

  let num_clauses             = range_option() in
  let num_literals_per_clause = range_option() in
  let num_vars                = range_option() in
  let ratio_vars              = float_option() in
  let num_funcs               = range_option() in
  let funcs_arity             = range_option() in
  let num_preds               = range_option() in
  let preds_arity             = range_option() in
  let max_depth               = int_option() in
  let seed                    = StdOpt.int_option() ~metavar:"<int>" in
  let incremental             = StdOpt.store_true() in

  let parser = make
    ~usage:("%prog "^subcommand^" [options...]")
    ~version:version_number
    ~formatter
    ()
  in

  add parser incremental             ~long_name:"incremental" ~short_name:'i' ~help:"Incremental mode";
  add parser num_clauses             ~long_name:"num-clauses"             ~help:"Number of clauses";
  add parser num_literals_per_clause ~long_name:"num-literals-per-clause" ~help:"Number of literals per clause";
  add parser num_vars                ~long_name:"num-vars"                ~help:"Number of total variables";
  add parser ratio_vars              ~long_name:"ratio-vars"              ~help:"Ratio of variables to all terms";
  add parser num_funcs               ~long_name:"num-funcs"               ~help:"Number of functions";
  add parser funcs_arity             ~long_name:"funcs-arity"             ~help:"Function arity";
  add parser num_preds               ~long_name:"num-preds"               ~help:"Number of predicates";
  add parser preds_arity             ~long_name:"preds-arity"             ~help:"Predicate arity";
  add parser max_depth               ~long_name:"max-depth"               ~help:"Maximum term depth";
  add parser seed                    ~long_name:"seed"                    ~help:"Random seed (optional)";

  (* let lst = parse_argv parser in *)
  (* prerr_string "Args: "; Array.print String.print stderr Sys.argv; prerr_newline(); *)
  let foooo = Array.tail Sys.argv 2 in
  (* prerr_string "Args: "; Array.print String.print stderr foooo; prerr_newline(); *)
  let lst = parse parser (foooo) in
  (* prerr_string "Options lst: "; List.print String.print stderr lst; prerr_newline(); *)

  try
    {
      options = {
        incremental             = Opt.get incremental;
        num_clauses             = Opt.get num_clauses;
        num_literals_per_clause = Opt.get num_literals_per_clause;
        num_vars                = Opt.get num_vars;
        ratio_vars              = Opt.get ratio_vars;
        num_funcs               = Opt.get num_funcs;
        funcs_arity             = Opt.get funcs_arity;
        num_preds               = Opt.get num_preds;
        preds_arity             = Opt.get preds_arity;
        max_depth               = Opt.get max_depth;
        seed                    = Opt.opt seed;
      };
      args = lst;
      parser = parser;
    }
  with 
  |Opt.No_value ->
    error parser ~status:1 "missing some mandatory argument(s); see -h for help";
    exit 1  (* Useless line, but above line returns unit instead of 'a so... *)

let solvers_parser params ~parser : (string * External.solver) list =
  let open BatOptParse.OptParser in

  let parse_solver s =
    let name, path = 
      try String.split s ~by:":" 
      with Not_found -> error parser ~status:1 "solver specification should be <solver>:<path>"; exit 1;
    in
    let solver = 
      match String.lowercase @@ String.trim name with
      | "iprover" -> External.iprover path
      | "vampire" | "vprover" -> External.vampire path
      | x -> error parser ~status:1 (Printf.sprintf "unrecognized solver '%s'" x); exit 1;
    in
    let short_path = 
      try snd @@ String.rsplit path ~by:"/" 
      with Not_found -> path
    in
    (short_path, solver)
  in
  List.map parse_solver params

(* ----- *)

let print_parser ~subcommand () = 
  let {options} = options_parser() ~subcommand in
  options

let compare_parser ~subcommand () : Options.t * (string * External.solver) list = 
  let {options; args=params; parser} = options_parser() ~subcommand in
  let solvers = solvers_parser params ~parser in
  (options, solvers)

let top_parser() : result = 
  let open BatOptParse in

  let parser = OptParser.make
    ~usage:(
      "%prog subcommand [options...]\n\n"
    ^ "Commands:\n"
    ^ "  print         Print a clauseset\n"
    ^ "  compare       Compare the answer of several programs\n"
    )
    ~version:version_number
    (* ~formatter *)
    ()
  in

  if Array.length Sys.argv = 1 then (
    OptParser.usage parser ();
    exit 1
  ) else (
    let lst = OptParser.parse parser (Array.sub Sys.argv 1 1) in
    (* prerr_string "Top lst: "; List.print String.print stderr lst; prerr_newline(); *)
    let subcommand = List.hd lst in
    match subcommand with
    | "print" -> 
      Print (print_parser() ~subcommand)
    | "compare" -> 
      let res = compare_parser() ~subcommand in
      Compare (fst res, snd res)
    | _ -> 
      OptParser.error parser (subcommand^" is not a subcommand."); exit 1
  )
