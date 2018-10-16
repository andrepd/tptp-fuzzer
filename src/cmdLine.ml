open Batteries

open Range

type result =
  | Print of Options.t
  | Incremental of Options.t
  | None

(* Helper functions *)
let parse_range (r: string) : int range =
  (* prerr_endline r; *)
  try
    Scanf.sscanf r "%d--%d" (--)
  with (* Scanf.Scan_failure _ | *) End_of_file ->
    Scanf.sscanf r "%d" (fun x -> x--x)

(* Fatal error: exception Ctype.Unify(_)  (wtf??) *)
(* let range_option() : int range BatOptParse.Opt.t = 
  let open BatOptParse.StdOpt in
  let (x: string BatOptParse.Opt.t) = str_option() ~metavar:"<range>" in
  let (old_get: unit -> string option) = x.option_get in
  let (new_get: unit -> int range option) = fun () -> Option.map parse_range (old_get()) in
  {x with option_get=new_get} *)

let int_option() =
  let open BatOptParse.StdOpt in
  int_option() ~metavar:"<number>"

let float_option() =
  let open BatOptParse.StdOpt in
  float_option() ~metavar:"<number>"

let formatter = 
  let terminal_width =
    try
      int_of_string (Sys.getenv "COLUMNS")    (* Might as well use it if it's there... *)
    with
    | Failure _ | Not_found -> 80
  in
  BatOptParse.Formatter.indented_formatter ~max_help_position:(terminal_width/2) ()



let print_parser() ~subcommand : Options.t = 
  let open BatOptParse in
  let open BatOptParse.OptParser in

  let num_clauses             = StdOpt.str_option() ~metavar:"<range>" in
  let num_literals_per_clause = StdOpt.str_option() ~metavar:"<range>" in
  let num_vars                = StdOpt.str_option() ~metavar:"<range>" in
  let ratio_vars              = float_option() in
  let num_funcs               = StdOpt.str_option() ~metavar:"<range>" in
  let funcs_arity             = StdOpt.str_option() ~metavar:"<range>" in
  let num_preds               = StdOpt.str_option() ~metavar:"<range>" in
  let preds_arity             = StdOpt.str_option() ~metavar:"<range>" in
  let max_depth               = int_option() in
  let seed                    = StdOpt.int_option() ~metavar:"<int>" in

  let parser = make
    ~usage:("%prog "^subcommand^" [options...]")
    ~version:"0.1"
    ~formatter
    ()
  in

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

  let lst = parse_argv parser in

  try
    {
      num_clauses             = Opt.get num_clauses |> parse_range;
      num_literals_per_clause = Opt.get num_literals_per_clause |> parse_range;
      num_vars                = Opt.get num_vars |> parse_range;
      ratio_vars              = Opt.get ratio_vars;
      num_funcs               = Opt.get num_funcs |> parse_range;
      funcs_arity             = Opt.get funcs_arity |> parse_range;
      num_preds               = Opt.get num_preds |> parse_range;
      preds_arity             = Opt.get preds_arity |> parse_range;
      max_depth               = Opt.get max_depth;
      seed                    = Opt.opt seed;
    }
  with BatOptParse.Opt.No_value -> (
    error parser ~status:1 "Missing some mandatory argument(s). See -h for help.";
    exit 1  (* Useless line, but above line returns unit instead of 'a so... *)
  )

let top_parser() : result = 
  let open BatOptParse in

  let parser = OptParser.make
    ~usage:(
      "%prog subcommand [options...]\n\n" ^
      "Commands:\n" ^
      "  print        Print a clauseset\n" ^
      "  incremental  Print a clauseset incrementally"
    )
    ~version:"0.1"
    ~formatter
    ()
  in

  if Array.length Sys.argv = 1 then (
    OptParser.usage parser ();
    exit 1
  ) else (
    let lst = OptParser.parse parser (Array.sub Sys.argv 1 1) in
    let subcommand = List.hd lst in
    match subcommand with
    | "print" -> 
      Print (print_parser() ~subcommand)
    | "incremental" -> 
      Incremental (print_parser() ~subcommand)
    | _ -> (
      OptParser.error parser (subcommand^" is not a subcommand."); exit 1
    )
  )
