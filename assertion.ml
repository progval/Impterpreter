type assertion =
    | True
    | False
    | LowerThan of Expr.expr*Expr.expr
    | GreaterThan of Expr.expr*Expr.expr
    | Equals of Expr.expr*Expr.expr
    | Not of assertion
    | And of assertion*assertion
    | Or of assertion*assertion

let rec string_of_assertion a =
    let aux1 s a b =
        String.concat "" [s; string_of_assertion a; ", "; string_of_assertion b; ")"]
    in
    let aux2 s a b =
        String.concat "" [s; Expr.string_of_expr a; ", "; Expr.string_of_expr b; ")"]
    in
    match a with
    | True -> "True"
    | False -> "False"
    | LowerThan(e1, e2) -> aux2 "LowerThan(" e1 e2
    | GreaterThan(e1, e2) -> aux2 "GreaterThan(" e1 e2
    | Equals(e1, e2) -> aux2 "Equals(" e1 e2
    | Not(e) -> String.concat "" ["Not("; string_of_assertion e; ")"]
    | And(a1, a2) -> aux1 "And(" a1 a2
    | Or(a1, a2) -> aux1 "Or(" a1 a2

let rec eval mem = function
    | True -> true
    | False -> false
    | LowerThan(e1, e2) -> Expr.eval mem e1 < Expr.eval mem e2
    | GreaterThan(e1, e2) -> Expr.eval mem e1 > Expr.eval mem e2
    | Equals(e1, e2) -> Expr.eval mem e1 = Expr.eval mem e2
    | Not(a) -> not (eval mem a)
    | And(a1, a2) -> (eval mem a1) && (eval mem a2)
    | Or(a1, a2) -> (eval mem a1) || (eval mem a2)
