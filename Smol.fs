module Smol

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Sublist of List<Expression>
    | Error of string

let tokenize (s: string) =
    s
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Split(' ')
    |> Array.toList
    |> List.filter (fun x -> x <> "")

let rec parse_each tokens =
    let rec loop_tail acc rest =
        match rest with
        | [] -> acc
        | x :: xs ->
            match x with
            | ")" ->
                (fst acc, List.skip 1 (snd acc)) // skip paren
            | "(" ->
                let (sublist, rest) = parse_each xs
                let listacc = fst acc
                loop_tail (listacc @ [ Sublist sublist ], rest) rest
            | _ ->
                let listacc = fst acc
                loop_tail (listacc @ [ Symbol x ], xs) xs

    loop_tail ([], tokens) tokens

let parse tokens =
    let (exprlist, rest) = parse_each tokens
    match exprlist with
    | [] -> Error "malformed input"
    | _ ->
        let ast =  List.head exprlist
        match ast with
        | Sublist y ->  ast
        | _ -> Error "missing opening parenthesis"

