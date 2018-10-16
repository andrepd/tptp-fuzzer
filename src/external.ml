open Batteries

type result = 
  | Sat
  | Unsat
  | Unknown

let result_of_string = function
  | Sat -> "Sat"
  | Unsat -> "Unsat"
  | Unknown -> "Unknown"

let search regex str =
  try
    ignore @@ Str.search_forward regex str 0;
    true
  with Not_found ->
    false

let iprover path =
  let command = Printf.sprintf "%s --stdin=true" path in
  (* let regex = Str.regexp "SZS status Satisfiable" in *)
  let regex_sat   = Str.regexp "% SZS status Satisfiable" in
  let regex_unsat = Str.regexp "% SZS status Unsatisfiable" in
  let func (problem: string) : result =
    (* prerr_endline "starting"; *)
    let output, input = Unix.open_process command in
    IO.nwrite input problem; IO.close_out input;
    (* prerr_endline "foo"; *)
    (* let lines = IO.lines_of output |> List.of_enum in *)
    let lines = IO.read_all output in
    (* prerr_endline "foo"; *)
    (* Unix.close_process (output, input); *)
    (* prerr_endline "foo"; *)
    (* let sat = lines |> List.exists (fun str ->
      search regex_sat str
    ) *)
    let sat = lines |> search regex_sat in
    let unsat = lines |> search regex_unsat in
    (* prerr_endline "ending"; *)
    if sat then
      Sat
    else if unsat then
      Unsat
    else
      Unknown

  in
  func

let vampire path =
  let command = Printf.sprintf "%s " path in
  (* let regex = Str.regexp "SZS status Satisfiable" in *)
  let regex_sat   = Str.regexp "% SZS status Satisfiable" in
                                  (* SZS status Satisfiable *)
  let regex_unsat = Str.regexp "% SZS status Unsatisfiable" in
                                  (* SZS status Unsatisfiable *)
  let func (problem: string) : result =
    let lines = 
      File.with_temporary_out (fun file name ->
        IO.nwrite file problem;
        let output = Unix.open_process_in (command ^ name) in
        IO.lines_of output
      )
    in

    try
      lines |> Enum.find_map (fun str ->
        if search regex_sat str then
          Some Sat
        else if search regex_unsat str then
          Some Unsat
        else
          None
      )
    with Not_found ->
      Unknown

  in
  func
