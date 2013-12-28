open Expr
open Assertion
open Com
open Function

let rec expr_step mem = function
  | Const k -> Const k
  | Var(s) -> Const (Memory.read mem s)

  | Add(Const i1, Const i2) -> Const(i1+i2)
  | Add(Const i1, e2) -> Add(Const i1, expr_step mem e2)
  | Add(e1, e2) -> Add(expr_step mem e1, e2)

  | Sub(Const i1, Const i2) -> Const(i1-i2)
  | Sub(Const i1, e2) -> Sub(Const i1, expr_step mem e2)
  | Sub(e1, e2) -> Sub(expr_step mem e1, e2)

  | Mul(Const i1, Const i2) -> Const(i1*i2)
  | Mul(Const i1, e2) -> Mul(Const i1, expr_step mem e2)
  | Mul(e1, e2) -> Mul(expr_step mem e1, e2)

  | Div(Const i1, Const i2) -> if i2 = 0 then
                                   raise DivisionByZero
                               else
                                   Const(i1 / i2)
  | Div(Const i1, e2) -> Div(Const i1, expr_step mem e2)
  | Div(e1, e2) -> Div(expr_step mem e1, e2)

  | RawCall(s, Const i1, Const i2) ->
          let (a1, a2, c) = Memory.get_function mem s in
          Call(Memory.write (Memory.write (Memory.zero_mem mem) a1 i1) a2 i2, c)
  | RawCall(s, Const i1, e2) -> RawCall(s, Const i1, expr_step mem e2)
  | RawCall(s, e1, e2) -> RawCall(s, expr_step mem e1, e2)

  | Call(mem, Return(Const i)) | Call(mem, Seq(Return(Const i), _)) -> Const i
  | Call(mem, c)-> let (mem, c) = com_step mem c in Call(mem, c)
  


and assertion_step mem = function
    | True -> True
    | False -> False

    | LowerThan(Expr.Const i1, Expr.Const i2) -> Assertion.assertion_of_bool (i1 < i2)
    | LowerThan(Expr.Const i1, e2) -> LowerThan(Expr.Const i1, expr_step mem e2)
    | LowerThan(e1, e2) -> LowerThan(expr_step mem e1, e2)

    | GreaterThan(Expr.Const i1, Expr.Const i2) -> Assertion.assertion_of_bool (i1 > i2)
    | GreaterThan(Expr.Const i1, e2) -> GreaterThan(Expr.Const i1, expr_step mem e2)
    | GreaterThan(e1, e2) -> GreaterThan(expr_step mem e1, e2)

    | Equals(Expr.Const i1, Expr.Const i2) -> Assertion.assertion_of_bool (i1 = i2)
    | Equals(Expr.Const i1, e2) -> Equals(Expr.Const i1, expr_step mem e2)
    | Equals(e1, e2) -> Equals(expr_step mem e1, e2)

    | Not(True) -> False
    | Not(False) -> True
    | Not(a) -> Not(assertion_step mem a)


    | And(True, True) -> True
    | And(True, False) | And(False, True) | And(False, False) -> False
    | And(True, a2) -> And(True, assertion_step mem a2)
    | And(False, a2) -> False
    | And(a1, a2) -> And(assertion_step mem a1, a2)

    | Or(False, False) -> False
    | Or(False, True) | Or(True, False) | Or(True, True) -> True
    | Or(False, a2) -> Or(False, assertion_step mem a2)
    | Or(True, a2) -> True
    | Or(a1, a2) -> Or(assertion_step mem a1, a2)

and com_step mem = function
  | Skip -> (mem, Skip)
  | Seq(c1, c2) -> (match (com_step mem c1) with
                   | (mem2, Skip) -> (mem2, c2)
                   | (mem2, c1) -> (mem2, Seq(c1, c2))
                   )
  | Print(s) -> print_int (Memory.read mem s); print_string "\n"; (mem, Skip)
  | Aff(s, Expr.Const i) -> (Memory.write mem s i, Skip)
  | Aff(s, e) -> (mem, Aff(s, expr_step mem e))
  | IfTE(Assertion.True, c, _) -> (mem, c)
  | IfTE(Assertion.False, _, c) -> (mem, c)
  | IfTE(b, c1, c2) -> (mem, IfTE(assertion_step mem b, c1, c2))
  | While(Assertion.True, b2, c) -> (mem, Seq(c, While(b2, b2, c)))
  | While(Assertion.False, b2, c) -> (mem, Skip)
  | While(b, b2, c) -> (mem, While(assertion_step mem b, b2, c))
  | Return(Const i) -> failwith "Return outside a function." (* If "return n" (with n: integer) has not been consumed by com_step it means we are not in a function. *)
  | Return(e) -> (mem, Return (expr_step mem e))

let rec exec mem = function
  | Skip -> mem
  | c -> match (com_step mem c) with (mem2, c2) -> exec mem2 c2
