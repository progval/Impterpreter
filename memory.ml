exception UndefinedFunction

type variables =
    | NoVar
    | Cons of string * int * variables
type 'com mem =
    | Memory of variables * ((string * 'com Function.function_) list)

let new_mem functions = Memory (NoVar, functions)
let zero_mem = function
    | Memory(variables, functions) -> Memory (NoVar, functions)
let string_of_mem =
    let rec aux mem acc =
        match mem with
        | NoVar -> acc
        | Cons(addr, value, mem2) -> aux mem2 (String.concat "" [addr; "="; string_of_int value] :: acc)
    in function Memory(variables, functions) -> String.concat "; " (aux variables [])

let rec read mem addr =
    let rec aux = function
    | NoVar -> 0
    | Cons (name, value, _) when name = addr -> value
    | Cons (_, _, variables2) -> aux variables2
    in let Memory(variables, functions) = mem
    in aux variables

let write mem addr v =
    let rec aux = function
    | NoVar -> Cons(addr, v, NoVar)
    | Cons(addr2, v2, mem2) when (addr=addr2) -> Cons(addr, v, mem2)
    | Cons(addr2, v2, mem2) -> Cons(addr2, v2, aux mem2)
    in let Memory(variables, functions) = mem
    in Memory(aux variables, functions)

let rec get_function_in_list functions name =
    match functions with
    | [] -> raise UndefinedFunction
    | ((name2, Function.Function(a1, a2, c)) :: _) when name=name2 -> (a1, a2, c)
    | (_ :: functions2) -> get_function_in_list functions2 name

let get_function mem name =
    let Memory(variables, functions) = mem
    in get_function_in_list functions name
