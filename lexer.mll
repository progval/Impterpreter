{
open Parser;;        (* le type "token" est défini dans parser.mli *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '\n'            { EOL }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['A'-'Z']+ as s { VAR s }
  | eof             { raise Eof } 
