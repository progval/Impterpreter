%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */

%left PLUS              /* associativité gauche, précédence minimale */
%left MINUS
%left TIMES
%left DIVIDE            /* associativité gauche, précédence maximale */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Expr.expr> main     /* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */
main:                       /* le point d'entrée */
    expr EOL                { $1 }  /* on veut reconnaître une expression */
;
expr:			    /* règles de grammaire pour les expressions */
  | INT                     { Const $1 }
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 } /* on récupère le deuxième élément */
  | expr PLUS expr          { Add($1,$3) }
  | expr MINUS expr         { Sub($1,$3) }
  | MINUS expr              { Sub(Const(0), $2) }
  | expr TIMES expr         { Mul($1,$3) }
  | expr DIVIDE expr        { Div($1,$3) }
;

