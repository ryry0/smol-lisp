module Smol

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Sublist of List<Expression>
    | Function of (List<Expression> -> Env -> Expression)
    | Error of string
and Env = Map<string, Expression>

//----------- builtins
let math op (args: Expression list) (env: Env) =
    let f x y =
        match (x, y) with
        | (Float x, Float y) -> Float (op x y)
        | (Integer x, Integer y) -> Integer <| int (op (float x) (float y))
        | _ -> Error $"Incorrect type"

    match args with
    | [] -> Error $"no arguments"
    | l ->
        List.reduce f l

//----------- Environment
let global_env =
    Env [
        ("+", Function <| math (+))
        ("-", Function <| math (-))
        ("*", Function <| math (*))
        ("/", Function <| math (/))
    ]

let lookup str env =
    match Map.tryFind str env with
    | Some x -> x
    | None -> Error $"Symbol lookup failure: {str}"

//----------- eval
let rec eval env expr =
    match expr with
    | Float _ | Integer _ as literal -> literal
    | Symbol str ->
        lookup str env
    | Sublist (h::t) ->
        let callable = eval env h
        let args = List.map (eval env) t
        match callable with
        | Function f -> f args env
        | Error str as error -> error
        | _ ->  Error "Not callable"
    | _ -> Error "Not implemented"

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
                let (exprlist, tok, num_parens) = acc
                (exprlist, List.skip 1 tok, num_parens - 1) // skip paren in token
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
            | Sublist y as res -> res
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

//helper to use in repl
let fsi_eval str =
    str
    |> tokenize
    |> parse
    |> eval global_env

let rec repl x =
    printf "smol>"

    let res =
        System.Console.ReadLine()
        |> tokenize
        |> parse
        |> to_string

    printfn "%s" res

    repl x
