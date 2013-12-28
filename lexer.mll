{
open Parser;;        (* le type "token" est d�fini dans parser.mli *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)

  (* Arithmetic *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '('             { LPAREN }
  | ')'             { RPAREN }

  (* Boolean expressions *)
  | "true"          { TRUE }
  | "false"         { FALSE }
  | '<'             { LOWERTHAN }
  | '>'             { GREATERTHAN }
  | '='             { EQUALS }
  | '~'             { NOT }
  | "and"           { AND }
  | "or"            { OR }

  (* Instructions *)
  | "skip"          { SKIP }
  | ";"             { SEMICOLON }
  | "print"         { PRINT }
  | ":="            { AFF }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "while"         { WHILE }
  | "do"            { DO }

  (* Function *)
  | "function"      { FUNCTION }
  | ","             { COMMA }
  | "return"        { RETURN }
  | "."             { DOT }

  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['A'-'Z']+ as s { VAR s }
  | ['a'-'z']+ as s { FUN s }

  | eof             { EOF }
