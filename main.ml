
let compile functions com =
    ignore (Step.exec (Memory.new_mem functions) com)

(* stdin d�signe l'entr�e standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on encha�ne les tuyaux: lexbuf est pass� � Lexer.token,
   et le r�sultat est donn� � Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let run () =
    try
        let (functions, com) = parse () in
        let errors = Analysis.check functions com in
            if (List.length errors) > 0 then
                ignore (List.map (function x -> print_string x; print_char '\n') errors)
            else
                compile functions com; flush stdout
    with
    | Parsing.Parse_error -> (print_string "erreur de saisie\n")
    | e -> print_string (String.concat "" ["Erreur : "; Printexc.to_string e; "\n"])
;;

let _ = run()
