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
let rec string_of_expr e =
  let aux s a b =
      String.concat "" [s; string_of_expr a; ", "; string_of_expr b; ")"]
  in
  match e with
  | Const k -> string_of_int k
  | Var s -> s
  | Add(e1,e2) -> aux "Add(" e1 e2
  | Sub(e1,e2) -> aux "Sub(" e1 e2
  | Mul(e1,e2) -> aux "Mul(" e1 e2
  | Div(e1,e2) -> aux "Div(" e1 e2

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

  
