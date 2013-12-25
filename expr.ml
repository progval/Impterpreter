exception DivisionByZero

(* un type pour des expressions arithmétiques simples *)
type expr =
  | Const of int
  | Var of string
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Div of expr*expr

(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  match e with
  | Const k -> print_int k
  | Var s -> print_string (String.concat "" ["Var("; s; ")"])
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Sub(e1,e2) -> aff_aux "Sub(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2

(* sémantique opérationnelle à grands pas *)
let rec eval mem = function
  | Const k -> k
  | Var(s) -> Memory.read mem s
  | Add(e1,e2) -> (eval mem e1) + (eval mem e2)
  | Sub(e1,e2) -> (eval mem e1) - (eval mem e2)
  | Mul(e1,e2) -> (eval mem e1) * (eval mem e2)
  | Div(e1,e2) -> let (num, denom) = (eval mem e1, eval mem e2) in
                  if denom = 0 then
                      raise DivisionByZero
                  else
                      num / denom

  
