%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE
%token SKIP SEMICOLON PRINT AFF
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */

%left PLUS              /* associativité gauche, précédence minimale */
%left MINUS
%left TIMES
%left DIVIDE            /* associativité gauche, précédence maximale */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Com.com> main     /* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */
main:                       /* le point d'entrée */
    com EOL                { $1 }  /* on veut reconnaître une expression */
;
expr:			    /* règles de grammaire pour les expressions */
  | INT                     { Expr.Const $1 }
  | VAR                     { Expr.Var $1 }
  | LPAREN expr RPAREN      { $2 } /* on récupère le deuxième élément */
  | expr PLUS expr          { Expr.Add($1,$3) }
  | expr MINUS expr         { Expr.Sub($1,$3) }
  | MINUS expr              { Expr.Sub(Const(0), $2) }
  | expr TIMES expr         { Expr.Mul($1,$3) }
  | expr DIVIDE expr        { Expr.Div($1,$3) }
;

/* Une commande qui n'est pas une séquence. */
com_notseq:
  | SKIP                    { Com.Skip }
  | PRINT VAR               { Com.Print($2) }
  | VAR AFF expr            { Com.Aff($1,$3) }
;
com:
  | com_notseq              { $1 }
  | com_notseq SEMICOLON com { Com.Seq($1,$3) }
  | com_notseq SEMICOLON    { Com.Seq($1,Com.Skip) }
