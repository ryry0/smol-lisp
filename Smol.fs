module Smol

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Sublist of List<Expression>
    | FloatFunction of (float -> float)
    | Error of string

type Environment = { env: Map<string, Expression> }

//----------- eval
let rec eval ast env = ()
//match ast with
//| Sublist l -> List.map eval l


let standard_env = ()
//{ env = Map
//   [
//      ("+",


//----------- Parsing
let tokenize (s: string) =
    s
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Split(' ')
    |> Array.toList
    |> List.filter (fun x -> x <> "")

let atomize (s: string) =
    try
        s |> int |> Integer
    with
    | :? System.FormatException ->
        try
            s |> float |> Float
        with
        | :? System.FormatException -> Symbol s

let rec parse_each tokens =
    let rec loop_tail acc rest =
        match rest with
        | [] -> acc
        | x :: xs ->
            match x with
            | ")" ->
                let (ast, tok, num_parens) = acc
                (ast, List.skip 1 tok, num_parens - 1) // skip paren in token
            | "(" ->
                let (sublist, rest, num_parens) = parse_each xs
                let (listacc, _, _) = acc
                loop_tail (listacc @ [ Sublist sublist ], rest, num_parens + 1) rest
            | _ ->
                let (listacc, _, num_parens) = acc
                loop_tail (listacc @ [ atomize x ], xs, num_parens) xs

    loop_tail ([], tokens, 0) tokens

let parse tokens =
    let (exprlist, rest, parens) = parse_each tokens

    match parens with
    | i when i < 0 -> Error $"Unexpected closing parenthesis"
    | i when i > 0 -> Error $"Missing {i} closing parentheses"
    | 0 ->
        match exprlist with
        | [] -> Error "malformed input"
        | _ ->
            match List.head exprlist with
            | Sublist _ -> Sublist exprlist
            | _ -> Error "missing opening parens"


let rec to_string expression =
    match expression with
    | Error s -> s
    | Sublist l ->
        match l with
        | [] -> " () "
        | l ->
            let inner = List.map to_string l |> List.reduce (+)
            " ( " + inner + " ) "
    | Float x -> string x
    | Integer x -> string x
    | Symbol s -> s

//---------REPL
let rec repl x =
    printf "smol>"

    let res =
        System.Console.ReadLine()
        |> tokenize
        |> parse
        |> to_string

    printfn "%s" res

    repl x
