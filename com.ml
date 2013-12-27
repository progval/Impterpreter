type com =
  | Skip
  | Seq of com*com
  | Print of string
  | Aff of string * Expr.expr
  | IfTE of Assertion.assertion*com*com
  | While of Assertion.assertion*com

let rec string_of_com c =
  match c with
  | Skip -> "Skip"
  | Seq(e1, e2) -> String.concat "" ["Seq("; string_of_com e1; ", "; string_of_com e2; ")"]
  | Print(s) -> String.concat "" ["Print("; s; ")"]
  | Aff(s, e) -> String.concat "" ["Aff("; s; ", "; Expr.string_of_expr e; ")"]
  | IfTE(b, c1, c2) -> String.concat "" ["IfTE("; Assertion.string_of_assertion b;
                       ", "; string_of_com c1; ", "; string_of_com c2; ")"]
  | While(b, c) -> String.concat "" ["While("; Assertion.string_of_assertion b;
                   ", "; string_of_com c; ")"]

let rec step mem = function
  | Skip -> (mem, Skip)
  | Seq(c1, c2) -> (match (step mem c1) with
                   | (mem2, Skip) -> (mem2, c2)
                   | (mem2, c1) -> (mem2, Seq(c1, c2))
                   )
  | Print(s) -> print_int (Memory.read mem s); print_string "\n"; (mem, Skip)
  | Aff(s, e) -> (Memory.write mem s (Expr.eval mem e), Skip)
  | IfTE(b, c1, c2) -> (mem, if (Assertion.eval mem b) then c1 else c2)
  | While(b, c) -> if (Assertion.eval mem b) then
                       (mem, Seq(c, While(b, c)))
                   else
                       (mem, Skip)

let rec exec mem = function
  | Skip -> mem
  | c -> match (step mem c) with (mem2, c2) -> exec mem2 c2
