let version_number = "0.0.2"

open Batteries

(* open Range *)

type result =
  | Print of Options.t
  | Compare of Options.t * (string * External.solver) list
  | Hammer of Options.t * (string * External.solver) list
  | Save of Options.t * int * string
  | Load of string * (string * External.solver) list

(* Helper functions *)
exception Bad_range
let parse_range (r: string) : int Range.t =
  (* prerr_endline r; *)
  try
    Scanf.sscanf r "%d--%d" Range.(--)
  with (* Scanf.Scan_failure _ | *) End_of_file -> try
    Scanf.sscanf r "%d" Range.point
  with (* Scanf.Scan_failure _ | *) End_of_file ->
    raise Bad_range

let range_option() : int Range.t BatOptParse.Opt.t = 
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

let bool_explicit_option() = 
  BatOptParse.Opt.value_option
    "<true|false>" (* metavar *)
    None (* default *)
    (function "true" -> true | "false" -> false | _ -> invalid_arg "must be true|false.") (* coerce *)
    (fun _ s -> Printf.sprintf "invalid value '%s' (must be true|false)" s) (* errfmt *)


let formatter = 
  let terminal_width = 
    try
      Int.of_string (Sys.getenv "COLUMNS")
    with
    | Failure _ | Not_found -> 80
  in
  BatOptParse.Formatter.indented_formatter ~max_help_position:(terminal_width/2) ()

let show_error ?(status=1) parser msg = 
  BatOptParse.OptParser.error parser ~status msg; 
  exit status;  (* Useless line, but above line returns unit instead of 'a so... *)

(* ----- *)

type options_parser_ret = {options: Options.t; args: string list; parser: BatOptParse.OptParser.t}
let options_parser() ~(usage_msg: string) = 
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
  let ratio_equality          = float_option() in
  let seed                    = StdOpt.int_option() ~metavar:"<int>" in
  let incremental             = StdOpt.store_true() in
  let file                    = StdOpt.str_option() in

  let parser = make
    ~usage:usage_msg
    ~version:version_number
    ~formatter
    ()
  in
  let mandatory = add_group parser "Mandatory options" in

  add parser incremental ~long_name:"incremental" ~short_name:'i' ~help:"Incremental mode";
  add parser seed        ~long_name:"seed"        ~short_name:'s' ~help:"Random seed (optional)";
  
  add parser ~group:mandatory num_clauses             ~long_name:"num-clauses"             ~help:"Number of clauses";
  add parser ~group:mandatory num_literals_per_clause ~long_name:"num-literals-per-clause" ~help:"Number of literals per clause";
  add parser ~group:mandatory num_vars                ~long_name:"num-vars"                ~help:"Number of total variables";
  add parser ~group:mandatory ratio_vars              ~long_name:"ratio-vars"              ~help:"Ratio of variables to all terms";
  add parser ~group:mandatory num_funcs               ~long_name:"num-funcs"               ~help:"Number of functions";
  add parser ~group:mandatory funcs_arity             ~long_name:"funcs-arity"             ~help:"Function arity";
  add parser ~group:mandatory num_preds               ~long_name:"num-preds"               ~help:"Number of predicates (beside equality)";
  add parser ~group:mandatory preds_arity             ~long_name:"preds-arity"             ~help:"Predicate arity";
  add parser ~group:mandatory ratio_equality          ~long_name:"ratio-equality"          ~help:"Ratio of equality vs other predicates";
  add parser ~group:mandatory max_depth               ~long_name:"max-depth"               ~help:"Maximum term depth";

  add parser file ~long_name:"file" ~help:"Read problems from file rather than generate anew";

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
        ratio_equality          = Opt.get ratio_equality;
        seed                    = Opt.opt seed;
      };
      args = lst;
      parser = parser;
    }
  with 
  |Opt.No_value ->
    show_error parser "missing some mandatory argument(s); see -h for help"

let solvers_parser params ~parser : (string * External.solver) list =
  let open BatOptParse.OptParser in

  let parse_solver s =
    let name, path = 
      try String.split s ~by:":" 
      with Not_found -> show_error parser "solver specification should be <solver>:<path>"
    in
    let solver = 
      match String.lowercase @@ String.trim name with
      | "iprover" -> External.iprover path
      | "vampire" | "vprover" -> External.vampire path
      | x -> show_error parser (Printf.sprintf "unrecognized solver '%s'" x)
    in
    let short_path = 
      try snd @@ String.rsplit path ~by:"/" 
      with Not_found -> path
    in
    (short_path, solver)
  in

  match params with
  | [] ->
    show_error parser ("no solvers supplied")
  | params ->
    List.map parse_solver params

(* ----- *)

let print_parser ~usage_msg () = 
  let {options} = options_parser() ~usage_msg in
  options

let compare_parser ~usage_msg () : Options.t * (string * External.solver) list = 
  let {options; args=params; parser} = options_parser() ~usage_msg in
  let solvers = solvers_parser params ~parser in
  (options, solvers)

let save_parser ~usage_msg () =
  let open BatOptParse.OptParser in
  let {options; args; parser} = options_parser() ~usage_msg in
  match args with
  | [n; dir] ->
    let n = Int.of_string n in
    (options, n, dir)
  | _ -> usage parser (); exit 1

let load_parser ~usage_msg () =
  let open BatOptParse.OptParser in
  let parser = make
    ~usage:usage_msg
    ~version:version_number
    ~formatter
    ()
  in
  let args = parse parser (Array.tail Sys.argv 2) in
  match args with
  | dir::(x::xs as solvers) ->
    (dir, solvers_parser solvers ~parser)
  | _ -> 
    usage parser (); exit 1

let top_parser() : result = 
  let open BatOptParse in

  let parser = OptParser.make
    ~usage:(
      "%prog <subcommand> [<options>]\n\n"
    ^ "Commands:\n"
    ^ "  print         Print a clauseset\n"
    ^ "  compare       Compare the answer of several programs\n"
    ^ "  hammer        As compare, but runs indefinitely and only reports discrepancies\n"
    ^ "  save          Saves a test set in a directory\n"
    ^ "  load          Loads and runs a test set in a directory"
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
      let usage_msg: string = "%prog print <options>" in
      Print (print_parser() ~usage_msg)
    | "compare" -> 
      let usage_msg: string = "%prog compare <options> (<solver>:<path>)..." in
      let res = compare_parser() ~usage_msg in
      Compare (fst res, snd res)
    | "hammer" -> 
      let usage_msg: string = "%prog hammer <options> (<solver>:<path>)..." in
      let res = compare_parser() ~usage_msg in
      Hammer (fst res, snd res)
    | "save" -> 
      let usage_msg: string = "%prog save <options> <number_of_formulas> <dir>" in
      let a,b,c = save_parser() ~usage_msg in
      Save (a,b,c)
    | "load" -> 
      let usage_msg: string = "%prog load <dir> (<solver>:<path>)..." in
      let dir, solvers = load_parser() ~usage_msg in
      Load (dir, solvers)
    | _ -> 
      show_error parser (Printf.sprintf "%s is not a subcommand." subcommand)
  )
