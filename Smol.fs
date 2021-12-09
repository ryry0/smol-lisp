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

let lookup str env =
    match Map.tryFind str env with
    | Some x -> x
    | None -> Error $"Symbol lookup failure: {str}"


//----------- eval
let rec eval env expr =
    match expr with
    | Float _
    | Integer _
    | Bool _ as literal -> (literal, env)
    | Symbol str -> (lookup str env, env)
    | Sublist (h :: args) ->
        let (callable, env_evaled) = eval env h

        match callable with
        | Function f -> f args env_evaled
        | Error str as error -> (error, env_evaled)
        | e -> (Error $"eval: {e} not callable", env_evaled)
    | _ -> (Error "eval: Not implemented", env)


let foldenv args env =
    //accumulates the results in list l while threading the environment through
    //the list
    let thread_env (l, e) x =
        let (res, res_e) = eval e x
        (l @ [ res ], res_e)

    List.fold thread_env ([], env) args

let foldenv_bind f args env =
    let (args_evaled, env_evaled) = foldenv args env
    let (res, f_env_res) = f args_evaled env_evaled
    (res, f_env_res)

//implementing comparisons and arith is great example of accidental complexity
//implemented separate equality comparison since typesystem coulnd't generalize float -> bool
//----------- builtins
let define (args: Expression list) env =
    match args with
    | [ Symbol name; value ] ->
        match Map.tryFind name env with
        | Some found -> (Error $"define: Symbol already defined {name}", env)
        | None ->
            let (res_value, res_env) = eval env value
            let new_env = Map.add name res_value res_env
            (Symbol name, new_env)
    | [] -> (Error "define: No name provided", env)
    | [ x ] -> (Error "define: Too few arguments to define", env)
    | _ -> (Error "define: Too many arguments to define", env)

let begin' args env =
    let (res, res_env) = foldenv args env
    (res |> List.rev |> List.head, res_env)

let if' (args: Expression list) (env: Env) : Expression * Env =
    match args with
    | []
    | [ _ ]
    | _ :: _ :: [] -> (Error "if: Not enough arguments", env)
    | cond :: conseq :: alt :: [] ->
        let (res, res_env) = eval env cond

        match res with
        | Bool x ->
            if x then
                eval res_env conseq
            else
                eval res_env alt
        | _ -> (Error "if: Condition does not return a boolean", env)


    | _ -> (Error "if: too many arguments", env)

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
    Env [ ("+", (+) |> math |> nop_env |> foldenv_bind |> Function)
          ("-", (-) |> math |> nop_env |> foldenv_bind |> Function)
          ("*", (*) |> math |> nop_env |> foldenv_bind |> Function)
          ("/", (/) |> math |> nop_env |> foldenv_bind |> Function)
          ("<",
           (<)
           |> comparison
           |> nop_env
           |> foldenv_bind
           |> Function)
          (">",
           (>)
           |> comparison
           |> nop_env
           |> foldenv_bind
           |> Function)
          ("=", equals |> nop_env |> foldenv_bind |> Function)
          ("<=",
           (<=)
           |> comparison
           |> nop_env
           |> foldenv_bind
           |> Function)
          (">=",
           (>=)
           |> comparison
           |> nop_env
           |> foldenv_bind
           |> Function)
          ("!=", not_equals |> nop_env |> foldenv_bind |> Function)
          ("if", Function if')
          ("begin", Function begin')
          ("define", define |> Function) ]

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

    let (res, new_env) =
        System.Console.ReadLine()
        |> tokenize
        |> parse
        |> eval env

    printfn $"{res}"

    repl new_env
