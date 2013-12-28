exception DivisionByZero

type 'com expr =
  | Const of int
  | Var of string
  | PlaceholderVar
  | Add of 'com expr*'com expr
  | Sub of 'com expr*'com expr
  | Mul of 'com expr*'com expr
  | Div of 'com expr*'com expr
  | RawCall of string * 'com expr * 'com expr
  | Call of 'com Memory.mem * 'com

(* fonction d'affichage *)
let rec string_of_expr e =
  let aux s a b =
      String.concat "" [s; string_of_expr a; ", "; string_of_expr b; ")"]
  in
  match e with
  | Const k -> string_of_int k
  | Var s -> s
  | PlaceholderVar -> failwith "PlaceholderVar outside function call."
  | Add(e1,e2) -> aux "Add(" e1 e2
  | Sub(e1,e2) -> aux "Sub(" e1 e2
  | Mul(e1,e2) -> aux "Mul(" e1 e2
  | Div(e1,e2) -> aux "Div(" e1 e2
  | RawCall(s, PlaceholderVar, PlaceholderVar) -> String.concat "" ["Call("; s; ")"]
  | RawCall(s, e1, PlaceholderVar) -> String.concat ""
        ["Call("; s; string_of_expr e1; ")"]
  | RawCall(s, e1, e2) -> String.concat "" ["Call("; s; ", ";
                       string_of_expr e1; ", "; string_of_expr e2; ")"]
  | Call(_, _) -> "<running function>"

(* sémantique opérationnelle à petits pas *)
