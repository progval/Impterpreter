
let compile functions com =
  begin
    ignore (Step.exec (Memory.new_mem functions) com);
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let run () =
  try
      let (functions, com) = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
    compile functions com; flush stdout
  with
  | Parsing.Parse_error -> (print_string "erreur de saisie\n")
  | e -> print_string (String.concat "" ["Erreur : "; Printexc.to_string e; "\n"])
;;

let _ = run()
