type com =
  | Skip
  | Seq of com*com
  | Print of string
  | Aff of string * Expr.expr

let rec string_of_com c =
  match c with
  | Skip -> "Skip"
  | Seq(e1, e2) -> String.concat "" ["Seq("; string_of_com e1; ", "; string_of_com e2; ")"]
  | Print(s) -> String.concat "" ["Print("; s; ")"]
  | Aff(s, e) -> String.concat "" ["Aff("; s; ", "; Expr.string_of_expr e; ")"]

let rec step mem = function
  | Skip -> (mem, Skip)
  | Seq(c1, c2) -> (match (step mem c1) with
                   | (mem2, Skip) -> (mem2, c2)
                   | (mem2, _) -> (mem2, Seq(c1, c2))
                   )
  | Print(s) -> print_int (Memory.read mem s); print_string "\n"; (mem, Skip)
  | Aff(s, e) -> (Memory.write mem s (Expr.eval mem e), Skip)

let rec exec mem = function
  | Skip -> mem
  | c -> match (step mem c) with (mem2, c2) -> exec mem2 c2
