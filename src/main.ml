open Batteries

open Range
open Generation

let is_infix x =
  x = "=" || x = "!="



let print options = 
  let gen = generate_clauseset options in
  Clauseset.print_tptp_string stdout gen

let print_incremental options =
  let gen = generate_clauseset_incremental options in
  Enum.print 
    (Clauseset.print_tptp_string) stdout gen
    ~first:"" ~last:"\n" ~sep:"\n\n%-----\n\n"

(* ----- *)

let compare (options: Options.t) (solvers: (string * External.solver) list) = 
  let gen = generate_clauseset options in
  let problem = Clauseset.to_string_tptp_string gen in

  solvers |> List.iter (fun (name, solver) ->
    let res = solver problem |> External.result_to_string in
    Printf.printf "%20s: %s\n" name res
  )

let compare_incremental (options: Options.t) (solvers: (string * External.solver) list) = 
  let gen = generate_clauseset_incremental options in

  print_string "     ";
  List.iter (Printf.printf "%20s " % fst) solvers;
  print_newline();

  gen |> Enum.iteri (fun i clauseset ->
    let problem = Clauseset.to_string_tptp_string clauseset in
    (* print_endline problem; *)

    Printf.printf "%4d " (i+1); IO.flush stdout;
    solvers |> List.iter (fun (_, solver) ->
      let res = solver problem |> External.result_to_string in
      Printf.printf "%20s " res; IO.flush stdout;
    );
    print_newline();
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
    let problem = Clauseset.to_string_tptp_string gen in

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
      let problem = Clauseset.to_string_tptp_string problem in

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

(* --- *)

(** Modified wrapper for mkdir: only creates if doesn't exist and nonempty *)
let mkdir dir =
  let dir_is_empty dir =
    let fd = Unix.opendir dir in
    try
      for i=0 to 2 do
        ignore @@ Unix.readdir fd;
      done;
      Unix.closedir fd;
      false
    with
    | End_of_file -> Unix.closedir fd; true
  in
  try
    Unix.mkdir dir 0o775
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> 
    if dir_is_empty dir then
      ()
    else
      failwith "directory exists and is nonempty"

let save options n dir = 
  let fname dir n = 
    Printf.sprintf "%s/test%d.p" dir n
  in

  mkdir dir;

  Enum.(1 -- n) |> Enum.iter (fun i ->
    let fname = fname dir i in
    let file = File.open_out fname in
    let gen = generate_clauseset options in
    Clauseset.print_tptp_string file gen;
    IO.close_out file;
  )

let save_incremental options n dir = 
  let fname dir n c = 
    Printf.sprintf "%s/test%d/%d.p" dir n c
  in

  mkdir dir;

  Enum.(1 -- n) |> Enum.iter (fun i ->
    let gen = generate_clauseset_incremental options in
    Unix.mkdir (Printf.sprintf "%s/test%d" dir i) 0o775;
    gen |> Enum.iteri (fun j clauseset ->
      let fname = fname dir i j in
      let file = File.open_out fname in
      Clauseset.print_tptp_string file clauseset;
      IO.close_out file;
    )
  )

(* --- *)

(** Recursively enumerates all files in directory *)
let rec listdir dir : string Enum.t =
  (* prerr_string "  " ; prerr_endline dir; *)
  let l = ref (Enum.empty()) in
  let fd = Unix.opendir dir in

  try
    while true do
      let f = ref (Unix.readdir fd) in
      while !f = "." || !f = ".." do
        f := Unix.readdir fd
      done;
      let f' = Printf.sprintf "%s/%s" dir !f in
      (* prerr_endline f'; *)
      if (Unix.stat f').st_kind = Unix.S_DIR then (
        l := Enum.append (!l) (listdir f')
      ) else (
        l := Enum.append (!l) (Enum.singleton f')
      )
    done; failwith "should not happen"

  with
  | End_of_file -> !l

let load dir solvers =
  let length_to_drop = String.length dir + 1 in

  print_string "                     ";
  List.iter (Printf.printf "%20s " % fst) solvers;
  print_newline();

  let files = listdir dir in
  files |> Enum.iter (fun fname ->
    let f = File.open_in fname in
    let problem = IO.read_all f in
    IO.close_in f;
    (* prerr_endline fname *)
    
    Printf.printf "%-20s " (String.lchop ~n:length_to_drop fname); IO.flush stdout;
    solvers |> List.iter (fun (_, solver) ->
      let res = solver problem |> External.result_to_string in
      Printf.printf "%20s " res; IO.flush stdout;
    );
    print_newline();
  )





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

  |CmdLine.Save (options, n, dir) ->
    Random.init (seed options);
    if not options.incremental then (
      save options n dir;
    ) else (
      save_incremental options n dir;
    )

  |CmdLine.Load (dir, solvers) ->
    (* Unix.chdir dir; *)
    let dir = 
      if String.ends_with dir "/" then
        String.rchop dir
      else 
        dir
    in
    load dir solvers
  end;

  ()

let () = main()
