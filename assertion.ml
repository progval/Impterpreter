type 'com assertion =
    | True
    | False
    | LowerThan of 'com Expr.expr*'com Expr.expr
    | GreaterThan of 'com Expr.expr*'com Expr.expr
    | Equals of 'com Expr.expr*'com Expr.expr
    | Not of 'com assertion
    | And of 'com assertion*'com assertion
    | Or of 'com assertion*'com assertion

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

let assertion_of_bool = function
    | true -> True
    | false -> False
