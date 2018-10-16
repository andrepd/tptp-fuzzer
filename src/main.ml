open Batteries

open Types
open Range

open Interfaces

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

let generate_clauseset (options: Options.t) : string clauseset =
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

  List.init num_clauses (fun _ -> generate_clause options ~funcs:list_funcs ~preds:list_preds ~num_vars)

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



let test() =
  let open Options in

  Random.init (Unix.gettimeofday() |> Int.of_float);

  let options = {
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
    let foo = IO.output_string() in
    let problem = Clauseset.print_tptp String.print foo clauseset; IO.close_out foo in
    let res_iprover = iprover problem |> External.result_of_string in
    let res_vampire = vampire problem |> External.result_of_string in
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
  let foo = IO.output_string() in
  let problem = Clauseset.print_tptp String.print foo last; IO.close_out foo in
  let res_iprover = iprover problem |> External.result_of_string in
  let res_vampire = vampire problem |> External.result_of_string in
  Printf.printf "%15s %15s\n" res_iprover res_vampire; *)

  ()

let seed (options: Options.t) =
  Option.(options.seed |? (Unix.gettimeofday() |> Int.of_float))

let main() =
  Random.init (Unix.gettimeofday() |> Int.of_float);

  begin match CmdLine.top_parser() with 
  | CmdLine.Print options -> (
    Random.init (seed options);
    let gen = generate_clauseset options in
    Clauseset.print_tptp String.print stdout gen
  )
  
  | CmdLine.Incremental options -> (
    Random.init (seed options);
    let gen = generate_clauseset_incremental options in
    Enum.print 
      ~first:"" ~last:"\n" ~sep:"\n\n---\n\n"
      (Clauseset.print_tptp String.print) stdout gen
  )

  (* | CmdLine. *)
  
  | CmdLine.None -> failwith "no subcommand"
  end;

  ()

let () = test()
