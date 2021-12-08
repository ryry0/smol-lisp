module Smol

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Sublist of List<Expression>
    | Error of string
    | Pair of Expression * Expression
    | Null

let unwrap_sublist (Sublist l) = l

(*
let rec parse tokens : Expression * List<string> =
    match tokens with
    | [] -> (Null , [])
    | x::xs ->
        match x with
        | "(" -> parseList xs
        | _ -> (Symbol x, xs)

and parseList tokens : Expression * List<string> =
    match tokens with
    | [] -> (Null, [])
    | x::xs ->
        match x with
        | ")" -> (Null, xs)
        | _ ->
            let (expr1, rest1) = parse tokens
            let (expr2, rest2) = parseList rest1
            ((Sublist [expr1; expr2]), rest2)

let rec parseSequence tokens : Expression =
    match tokens with
    | [] -> Sublist []
    | _  ->
        let (expr, rest) = parse tokens
        let sublist = unwrap_sublist <| parseSequence rest
        Sublist (expr :: sublist)


//make tail call recursive
let rec read_from acc tokens : (Expression, List<string>) =
    match tokens with
    | [] -> Sublist []
    | x::xs ->
        match x with
        | _ ->
            let sublist = read_from xs
            let pure_sublist = unwrap_sublist sublist
            Symbol x :: pure_sublist |> Sublist
            *)

//4 3 2 1
let rec loop (x: int) =
    match x with
    | less_eq_zero when less_eq_zero <= 0 -> []
    | positive -> positive :: loop (positive - 1)

//1 2 3 4
let rec revloop (x: int) =
    match x with
    | less_eq_zero when less_eq_zero <= 0 -> []
    | positive -> revloop (positive - 1) @ [ positive ]

//3 2 1 4
let rev1loop (x: int) =
    match x with
    | less_eq_zero when less_eq_zero <= 0 -> []
    | positive -> loop (positive - 1) @ [ positive ]

// 1 2 1 2 3 4
let rec rev2loop (x: int) =
    match x with
    | less_eq_zero when less_eq_zero <= 0 -> []
    | a when a < 2 -> a :: loop (a + 1)
    | b -> rev2loop (b - 1) @ [ b ]


//1 2 3 4
let loop_tailcaller (x: int) =
    let rec loop_tail acc (z: int) : List<int> =
        match z with
        | less_eq_zero when less_eq_zero <= 0 -> acc
        | positive -> loop_tail (positive :: acc) (positive - 1)

    loop_tail [] x

//4 3 2 1
let revloop_tailcaller (x: int) =
    let rec loop_tail acc (z: int) : List<int> =
        match z with
        | less_eq_zero when less_eq_zero <= 0 -> acc
        | positive -> loop_tail (acc @ [ positive ]) (positive - 1)

    loop_tail [] x

let effrevloop_tailcaller (x: int) =
    let rec loop_tail acc (z: int) : List<int> =
        match z with
        | less_eq_zero when less_eq_zero <= 0 -> acc
        | positive -> loop_tail (positive :: acc) (positive - 1)

    loop_tail [] x |> List.rev

let parse_until_close tokens =
    let rec loop_tail acc rest =
        match rest with
        | [] -> acc
        | x :: xs ->
            match x with
            | ")" -> (fst acc, List.skip 1 (snd acc)) // skip paren
            | _ ->
                let listacc = fst acc
                loop_tail (listacc @ [ Symbol x ], xs) xs

    loop_tail ([], tokens) tokens

let parse_each tokens =
    let rec loop_tail acc rest =
        match rest with
        | [] -> acc
        | x :: xs ->
            match x with
            | "(" ->
                parse_until_close xs
            | _ ->
                let listacc = fst acc
                loop_tail (listacc @ [ Symbol x ], xs) xs

    loop_tail ([], tokens) tokens


(*
let rec parse_one tokens : Expression * List<string> =
    match tokens with
    | [] -> (Sublist [], [])
    | x::xs ->
        match x with
        | "(" ->
            parse_until_close xs

        | _ ->
            (Symbol x, xs)


and parse_until_close rest : Expression * List<string> =
    match rest with
    | [] -> (Sublist [], [])
    | x::xs ->
        match x with
        | _ ->
            let (expr, rest) = parse_one xs
            let (expr2, rest2) = parse_until_close rest

        | ")" -> (Sublist [], xs)

let rec parse tokens : Expression =
    match tokens with
    | [] -> Sublist []
    | _ ->
        let (expr, rest) = parse_one tokens
        expr :: parse rest

    let rec loop (acc : AST) (n : List<string>) =
        match n with
        | [] -> acc
        | x::xs ->
            let (new_element, length) =
                match x with
                | "(" ->
                    let sublist =
                        List.takeWhile (fun x -> x <> ")") xs
                    ([Sublist (loop [] sublist)], List.length sublist)
                | ")" -> ([], 0)
                | _ -> ([Symbol x], 0)
            let (Sublist acc_deconstructed) = acc
            loop (Sublist (acc_deconstructed @ new_element)) <| List.skip length xs

    loop (Sublist []) tokens
    *)

let tokenize (s: string) =
    s
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Split(' ')
    |> Array.toList
    |> List.filter (fun x -> x <> "")
