
let compile functions com =
    ignore (Step.exec (Memory.new_mem functions) com)

let lexbuf = Lexing.from_channel stdin

let parse () = Parser.main Lexer.token lexbuf

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
