%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> VAR
%token TRUE FALSE LOWERTHAN GREATERTHAN EQUALS NOT AND OR
%token PLUS MINUS TIMES DIVIDE
%token SKIP SEMICOLON PRINT AFF
%token IF THEN ELSE
%token WHILE DO
%token LPAREN RPAREN
%token EOF

%nonassoc LOWERTHAN GREATERTHAN EQUALS

%right NOT
%left AND OR

%left PLUS MINUS
%left TIMES DIVIDE

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Com.com> main     /* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */
main:                       /* le point d'entrée */
    com EOF                 { $1 }  /* on veut reconnaître une expression */
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

assertion:
  | TRUE                    { Assertion.True }
  | FALSE                   { Assertion.False }
  | expr LOWERTHAN expr     { Assertion.LowerThan($1,$3) }
  | expr GREATERTHAN expr   { Assertion.GreaterThan($1,$3) }
  | expr EQUALS expr        { Assertion.Equals($1,$3) }
  | NOT assertion           { Assertion.Not($2) }
  | assertion AND assertion { Assertion.And($1,$3) }
  | assertion OR assertion  { Assertion.Or($1,$3) }

com_base:
  | SKIP                    { Com.Skip }
  | PRINT VAR               { Com.Print($2) }
  | VAR AFF expr            { Com.Aff($1,$3) }
  | LPAREN com RPAREN       { $2 }
  | WHILE assertion DO com_base { Com.While($2,$4) }
  | WHILE assertion DO com_if { Com.While($2,$4) }
;
com_seq:
  | com_if SEMICOLON com    { Com.Seq($1,$3) }
  | com_if SEMICOLON        { Com.Seq($1,Com.Skip) }
;

com_paired_if:
  | com_base                { $1 }
  | IF assertion THEN com_paired_if ELSE com_paired_if  { Com.IfTE($2,$4,$6) }
;
com_unpaired_if:
  | IF assertion THEN com_paired_if ELSE com_unpaired_if  { Com.IfTE($2,$4,$6) }
  | IF assertion THEN com_base { Com.IfTE($2,$4,Com.Skip) }
  | IF assertion THEN com_unpaired_if { Com.IfTE($2,$4,Com.Skip) }
;
com_if:
  | com_paired_if           { $1 }
  | com_unpaired_if         { $1 }
;

com:
  | com_seq                 { $1 }
  | com_if                  { $1 }
;
