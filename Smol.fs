module Smol

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Bool of bool
    | Sublist of List<Expression>
    | Function of (List<Expression> -> Env -> Expression * Env)
    | Error of string

and Env = Map<string, Expression>

//implementing comparisons and arith is great example of accidental complexity
//implemented separate equality comparison since typesystem coulnd't generalize float -> bool
//----------- builtins
let define (args: Expression list) env =
    printfn $"{args}"
    match args with
    | [Symbol name; value] ->
        match Map.tryFind name env with
        | Some found -> (Error $"define: Symbol already defined {found}", env)
        | None ->
            let new_env = Map.add name value env
            (Sublist [], new_env)
    | [] -> (Error "define: No name provided", env)
    | [x] -> (Error "define: Too few arguments to define", env)
    | _ -> (Error "define: Too many arguments to define", env)


//convenience function for returning results when operation does nothing to env
let nop_env f args env =
    let res = f args env
    (res, env)

let math op (args: Expression list) (env: Env) =
    let f x y =
        match (x, y) with
        | (Float x, Float y) -> Float(op x y)
        | (Integer x, Integer y) -> Integer <| int (op (float x) (float y))
        | _ -> Error $"op: Incorrect type"

    match args with
    | [] -> Error $"op: no arguments"
    | l -> List.reduce f l

let comparison op (args: Expression list) (env: Env) =
    let f x y =
        match (x, y) with
        | (Float x, Float y) -> Bool(op x y)
        | (Integer x, Integer y) -> Bool(op x y)
        | _ -> Error $"Incorrect or mismatched type"


    match args with
    | [] -> Error $"no arguments"
    | l -> List.reduce f args

let equals (args: Expression list) (env: Env) =
    let f x y =
        match (x, y) with
        | (Float x, Float y) -> Bool((=) x y)
        | (Integer x, Integer y) -> Bool((=) x y)
        | (Bool x, Bool y) -> Bool((=) x y)
        | _ -> Error $"Incorrect or mismatched type"


    match args with
    | [] -> Error $"no arguments"
    | l -> List.reduce f args

let not_equals (args: Expression list) (env: Env) =
    match equals args env with
    | Bool x -> Bool(not x)
    | Error _ as error -> error
    | _ -> Error "notequals"

//----------- Environment
let global_env =
    Env [ ("+", Function <| nop_env (math (+)))
          ("-", Function <| nop_env (math (-)))
          ("*", Function <| nop_env (math (*)))
          ("/", Function <| nop_env (math (/)))
          ("<", Function <| nop_env (comparison (<)))
          (">", Function <| nop_env (comparison (>)))
          ("=", Function <| nop_env (equals))
          ("<=", Function <| nop_env (comparison (<=)))
          (">=", Function <| nop_env (comparison (>=)))
          ("!=", Function <| nop_env (not_equals))
          ("define", Function <| define) ]

let lookup str env =
    match Map.tryFind str env with
    | Some x -> x
    | None -> Error $"Symbol lookup failure: {str}"

//----------- eval
let rec eval env expr =
    //accumulates the results in list l while threading the environment through
    //the list
    let thread_env (l,e) x =
        let (res, res_e) = eval e x
        (l @ [res], res_e)

    match expr with
    | Float _
    | Integer _
    | Bool _ as literal -> (literal, env)
    | Symbol str -> (lookup str env, env)
    | Sublist (h :: t) ->
        let (callable, env1) = eval env h
        let (args, env2) = List.fold thread_env ([], env1) t

        match callable with
        | Function f -> f args env2
        | Error str as error -> (error, env2)
        | _ -> (Error "Not callable", env2)
    | _ -> (Error "Not implemented", env)

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
        | :? System.FormatException ->
            match s with
            | "true" -> Bool true
            | "false" -> Bool false
            | _ -> Symbol s

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
    | Bool x ->
        match x with
        | true -> "true"
        | false -> "false"

//---------REPL

//helper to use in repl
let fsi_eval str =
    str |> tokenize |> parse |> eval global_env

let rec repl env =
    printf "smol>"

    let res =
        System.Console.ReadLine()
        |> tokenize
        |> parse
        |> eval global_env

    printfn $"{res}"

    repl env
