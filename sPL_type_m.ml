(* PLEASE DO NOT CHANGE THIS FILE *)

open SPL_type
open Debug.Basic
open SPLc
module S = SPL


(* let set_source_file (arg:string) =  *)
(*   source_files := arg :: !source_files *)

(* let process_cmd_line () =  *)
(*   Arg.parse option_flag set_source_file usage *)

let usage = "usage: " ^ Sys.argv.(0) ^ " [options] <filename>"

(* calling sPL parser *)
let parse_file (filename:string) : (string * S.sPL_expr) =
  SPL_parser.parse_file filename

(* set up for command argument
   using Sys and Arg modules *)

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse [] (fun s -> file := s) usage; 
  if String.length !file == 0 then print_endline usage else 
    let _ = print_endline "LOADING sPL program .." in
    let (s,p) = parse_file !file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline (" AS ==> "^(S.string_of_sPL p)) in
    let _ = print_endline "TYPE CHECKING program .." in
    let (v,np) = type_infer [] p in
    match v with
      | None -> print_endline " ==> type error detected"
      | Some t ->
            begin
              print_endline (" ==> inferred type "^(S.string_of_sPL_type t));
              let _ = print_string "TRANSFORMING ==> " in
              let np = trans_exp np in
              let _ = print_endline (string_of_sPL np) in
              ()
            end
