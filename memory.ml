type mem = NullMem | Cons of string * int * mem

let new_mem () = NullMem
let zero_mem mem = NullMem
let string_of_mem mem =
    let rec aux mem acc =
        match mem with
        | NullMem -> acc
        | Cons(addr, value, mem2) -> aux mem2 (String.concat "" [addr; "="; string_of_int value] :: acc)
    in String.concat "; " (aux mem [])

let rec read mem addr =
    match mem with
    | NullMem -> 0
    | Cons (name, value, _) when name = addr -> value
    | Cons (_, _, mem2) -> read mem2 addr

let write mem addr v =
    let rec aux = function
    | NullMem -> Cons(addr, v, NullMem)
    | Cons(addr2, v2, mem2) when (addr=addr2) -> Cons(addr, v, mem2)
    | Cons(addr2, v2, mem2) -> Cons(addr2, v2, aux mem2)
    in aux mem
