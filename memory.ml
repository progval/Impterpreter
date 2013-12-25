type mem = NullMem | Const of string * int * mem

let new_mem () = NullMem
let zero_mem mem = NullMem

let rec read mem addr =
    match mem with
    | NullMem -> 0
    | Const (name, value, _) when name = addr -> value
    | Const (_, _, mem2) -> read mem2 addr
