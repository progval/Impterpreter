%{
(* --- pr�ambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lex�mes */

%token <int> INT       /* le lex�me INT a un attribut entier */
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token EOL             /* retour � la ligne */

%left PLUS              /* associativit� gauche, pr�c�dence minimale */
%left MINUS
%left TIMES
%left DIVIDE            /* associativit� gauche, pr�c�dence maximale */

%start main             /* "start" signale le point d'entr�e: c'est ici main */
%type <Expr.expr> main     /* on _doit_ donner le type du point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */
main:                       /* le point d'entr�e */
    expr EOL                { $1 }  /* on veut reconna�tre une expression */
;
expr:			    /* r�gles de grammaire pour les expressions */
  | INT                     { Const $1 }
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 } /* on r�cup�re le deuxi�me �l�ment */
  | expr PLUS expr          { Add($1,$3) }
  | expr MINUS expr         { Sub($1,$3) }
  | MINUS expr              { Sub(Const(0), $2) }
  | expr TIMES expr         { Mul($1,$3) }
  | expr DIVIDE expr        { Div($1,$3) }
;

