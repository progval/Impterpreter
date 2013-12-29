
(* Returns the number of arguments of a function *)
let function_num_args = function (a1, a2, s) ->
    if a1 = "" then 0 else (if a2 = "" then 1 else 2)
(* Returns the number of arguments of a function call *)
let call_num_args = function
    | Expr.RawCall(s, Expr.PlaceholderVar, Expr.PlaceholderVar) -> 0
    | Expr.RawCall(s, _, Expr.PlaceholderVar) -> 1
    | Expr.RawCall(s, _, _) -> 2
    | _ -> failwith "call_num_args should only be called on a RawCall node."

(* Takes the list of functions and an expression, and checks if all
 * functions calls have the right number of arguments. *)
let rec check_function_calls_in_expr functions = function
    | Expr.Const k -> []
    | Expr.Var s -> []
    | Expr.PlaceholderVar -> failwith "PlaceholderVar outside a function call."
    | Expr.Add(e1, e2) | Expr.Sub(e1, e2) | Expr.Mul(e1, e2) | Expr.Div(e1, e2) ->
            (check_function_calls_in_expr functions e1) @
            (check_function_calls_in_expr functions e1)
    | Expr.RawCall(s, _, _) as rc -> (
            try
                let f = (Memory.get_function_in_list functions s) in
                let expected = function_num_args f and got = call_num_args rc in
                if (expected = got) then
                    []
                else
                    [String.concat " " ["Function"; s; "expects"; string_of_int expected; "arguments, but gets"; string_of_int got]]
            with
            | Memory.UndefinedFunction -> [String.concat " " ["Function"; s; "is called but is not defined"]]
        )
    | Expr.Call(_, _) -> failwith "Function calls should not have been converted at this point."

(* Takes the list of functions and an assertion, and checks if all
 * functions calls have the right number of arguments. *)
let rec check_function_calls_in_assertion functions = function
    | Assertion.True | Assertion.False -> []
    | Assertion.LowerThan(e1, e2) | Assertion.GreaterThan(e1, e2) | Assertion.Equals(e1, e2) ->
            (check_function_calls_in_expr functions e1) @
            (check_function_calls_in_expr functions e2)
    | Assertion.And(a1, a2) | Assertion.Or(a1, a2) ->
            (check_function_calls_in_assertion functions a1) @
            (check_function_calls_in_assertion functions a2)
    | Assertion.Not(a) -> check_function_calls_in_assertion functions a


(* Takes the list of functions and a command, and checks if all
 * functions calls have the right number of arguments. *)
let rec check_function_calls_in_com functions = function
    | Com.Skip -> []
    | Com.Print s -> []
    | Com.Aff(_, e) | Com.Return(e) -> check_function_calls_in_expr functions e
    | Com.Seq(c1, c2) ->
            (check_function_calls_in_com functions c1) @
            (check_function_calls_in_com functions c2)
    | Com.IfTE(b, c1, c2) ->
            (check_function_calls_in_assertion functions b) @
            (check_function_calls_in_com functions c1) @
            (check_function_calls_in_com functions c2)
    | Com.While(b, b2, c) ->
            if b = b2 then
                (check_function_calls_in_assertion functions b) @
                (check_function_calls_in_com functions c)
            else
                failwith "While statement has started to be evaluated (or there is a bug in the grammar definition)."

(* Check all function calls in a program. *)
let check_function_calls functions com =
    (check_function_calls_in_com functions com) @
    (List.concat (List.map (function (_, Function.Function(_, _, c)) -> check_function_calls_in_com functions c) functions))

let check functions com =
    check_function_calls functions com
