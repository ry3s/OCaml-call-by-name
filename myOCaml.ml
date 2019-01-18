open Syntax
open MyParser
open MyLexer
open Eval
open Print
open Type
   
let myml  in_chan =
  let lexbuf = Lexing.from_channel in_chan in 
  let rec repl env tenv = 
    try
      let result = MyParser.main MyLexer.token lexbuf in
      let (t_list, t_newenv) = infer_cmd tenv result in
      let (newenv, x) = execute_cmd env result in
      (match x with
      | Left v -> 
         List.iter (fun t  ->
             print_string "- : ";
             print_type t;
             print_string " = ";
             print_value v;
             print_newline ();
           ) t_list;
      | Right () ->
         List.iter (fun t  ->
             print_string "- : ";
             print_type t;
             print_string " = ";
             print_string "<lazy>";
             print_newline ();
           ) t_list;
      flush stdout;);
      repl newenv  t_newenv
	  with
    | Eval.End -> ()
    | Type.End -> ()
    | Parsing.Parse_error -> 
      print_endline "Parse Error!" in
  repl [] []  
;;

let main () = if Array.length Sys.argv = 1 then
                myml stdin
              else
                let ic = open_in Sys.argv.(1) in
                myml ic
;;

if !Sys.interactive then 
  ()
else 
  main ()
    
  
