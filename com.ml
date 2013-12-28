type com =
  | Skip
  | Seq of com*com
  | Print of string
  | Aff of string * com Expr.expr
  | IfTE of com Assertion.assertion*com*com
  | While of com Assertion.assertion*com Assertion.assertion*com
  | Return of com Expr.expr


let rec string_of_com c =
  match c with
  | Skip -> "Skip"
  | Seq(e1, e2) -> String.concat "" ["Seq("; string_of_com e1; ", "; string_of_com e2; ")"]
  | Print(s) -> String.concat "" ["Print("; s; ")"]
  | Aff(s, e) -> String.concat "" ["Aff("; s; ", "; Expr.string_of_expr e; ")"]
  | IfTE(b, c1, c2) -> String.concat "" ["IfTE("; Assertion.string_of_assertion b;
                       ", "; string_of_com c1; ", "; string_of_com c2; ")"]
  | While(b, b2, c) -> String.concat "" ["While("; Assertion.string_of_assertion b;
                   ", "; string_of_com c; ")"]
  | Return(e) -> String.concat "" ["Return("; Expr.string_of_expr e; ")"]

