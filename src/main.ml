open Batteries

open Range
open Generation



let print options = 
  let gen = generate_clauseset options in
  Clauseset.print_tptp String.print stdout gen

let print_incremental options =
  let gen = generate_clauseset_incremental options in
  Enum.print 
    (Clauseset.print_tptp String.print) stdout gen
    ~first:"" ~last:"\n" ~sep:"\n\n%-----\n\n"

(* ----- *)

let compare (options: Options.t) (solvers: (string * External.solver) list) = 
  let gen = generate_clauseset options in
  let problem = Clauseset.to_string_tptp identity gen in

  solvers |> List.iter (fun (name, solver) ->
    let res = solver problem |> External.result_to_string in
    Printf.printf "%20s: %s\n" name res
  )

let compare_incremental (options: Options.t) (solvers: (string * External.solver) list) = 
  let gen = generate_clauseset_incremental options in

  List.iter (Printf.printf "     %20s " % fst) solvers;
  print_newline();

  gen |> Enum.iteri (fun i clauseset ->
    let problem = Clauseset.to_string_tptp identity clauseset in
    (* print_endline problem; *)

    Printf.printf "%4d " (i+1);
    solvers |> List.iter (fun (_, solver) ->
      let res = solver problem |> External.result_to_string in
      Printf.printf "%20s " res
    );
    print_newline()
  )

(* ----- *)

let discrepancy l =
  let sat = ref false in
  let unsat = ref false in
  l |> List.iter (function
    |_, External.Sat -> 
      sat := true
    |_, External.Unsat -> 
      unsat := true
    |_, External.Unknown -> 
      ()
  );
  !sat && !unsat

let hammer options solvers = 
  Enum.range 0 |> Enum.iter (fun i ->
    Printf.eprintf "\rIt: %d" i; flush stderr;

    let gen = generate_clauseset options in
    let problem = Clauseset.to_string_tptp identity gen in

    let results = 
      solvers |> List.map (fun (name, solver) -> (name, solver problem))
    in

    if discrepancy results then (
      Printf.printf "%% [It: %d] Discrepancy in problem.\n" i;
      results |> List.iter (fun (name, result) ->
        Printf.printf "%%  %17s: %s\n" name (External.result_to_string result)
      );

      print_endline "\n% Problem:\n";
      print_endline problem;
      print_endline "\n% End problem.\n";
    );
  )

let hammer_incremental options solvers =
  Enum.range 0 |> Enum.iter (fun i ->
    Printf.eprintf "\rIt: %d" i; flush stderr;

    let gen = generate_clauseset_incremental options in
    gen |> Enum.iteri (fun j problem ->
      let problem = Clauseset.to_string_tptp identity problem in

      let results = 
        solvers |> List.map (fun (name, solver) -> (name, solver problem))
      in

      (* if List.hd results |> snd = External.Unsat then ( *)
      if discrepancy results then (
        Printf.printf "%% [It: %d] Discrepancy in problem, at clauses=%d.\n" i (succ j);
        results |> List.iter (fun (name, result) ->
          Printf.printf "%%  %17s: %s\n" name (External.result_to_string result)
        );

        print_endline "\n% Problem: %\n";
        print_endline problem;
        print_endline "\n% End problem. %\n";
      );
    )
  )


let test() =
  let open Options in

  Random.init (Unix.gettimeofday() |> Int.of_float);

  let options = {
    incremental = true;

    num_clauses = (* 5--8; *) 20--20;
    num_literals_per_clause = 1--4;

    num_vars = 3--3;
    ratio_vars = 0.315;
    
    num_funcs = 3--3;
    funcs_arity = 0--3;
    
    num_preds = 3--3;
    preds_arity = 1--3;
    
    max_depth = 3;

    seed = None;
  }
  in

  (* let options = CmdLine.print_parser() in

  let gen = generate_clauseset_incremental options |> List.of_enum in

  List.print ~first:"" ~last:"\n" ~sep:"\n\n---\n\n" (Clauseset.print_tptp String.print) stdout gen;
  
  if not (List.for_all (Clauseset.validate_tptp String.print) gen) then
    failwith "invalid tptp"; *)

  let gen = generate_clauseset_incremental options in

  let iprover = External.iprover "../iprover/iproveropt" in
  let vampire = External.vampire "../vampire/vampire4.2.2_noz3" in

  Printf.printf "%15s %15s\n" ("iProver 2.8") ("Vampire 4.2.2");
  gen |> Enum.iter (fun clauseset ->
    let problem = Clauseset.to_string_tptp identity clauseset in
    (* print_endline problem; *)
    let res_iprover = iprover problem |> External.result_to_string in
    let res_vampire = vampire problem |> External.result_to_string in
    Printf.printf "%15s %15s\n" res_iprover res_vampire
  );

  let last = generate_clauseset options in
  (* Clauseset.print_tptp String.print stdout last; *)

  (* let test: string clauseset = [
    [{sign=true;  atom=Pred("p",[])}]; 
    [{sign=false; atom=Pred("p",[])}]; 
  ]
  in
  Clauseset.print_tptp String.print stdout test;
  let problem = Clauseset.to_string_tptp identity last in
  let res_iprover = iprover problem |> External.result_to_string in
  let res_vampire = vampire problem |> External.result_to_string in
  Printf.printf "%15s %15s\n" res_iprover res_vampire; *)

  ()

let seed (options: Options.t) =
  let default_seed() = Unix.gettimeofday() |> Int.of_float in
  Option.(options.seed |? default_seed())

let main() =
  begin match CmdLine.top_parser() with 
  |CmdLine.Print options -> 
    Random.init (seed options);
    if not options.incremental then (
      print options
    ) else (
      print_incremental options
    )

  |CmdLine.Compare (options, solvers) ->
    Random.init (seed options);
    if not options.incremental then (
      compare options solvers
    ) else (
      compare_incremental options solvers
    )

  |CmdLine.Hammer (options, solvers) ->
    Random.init (seed options);
    if not options.incremental then (
      hammer options solvers
    ) else (
      hammer_incremental options solvers
    )
  end;

  ()

let () = main()
