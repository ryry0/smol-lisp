module Smol

//currently lambda closures are c-like
//to do dynamic closures you need to keep function stack associated with
//function
type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Bool of bool
    | Sublist of List<Expression>
    | Function of (List<Expression> -> Env -> Expression * Env)
    //| Procedure of Function * Env
    | Error of string

and Frame = Map<string, Expression>
and Env = Frame list

let lookup str env =
    //recursive lookup
    let rec lookup_tail envlist =
        match envlist with
        | [] -> Error $"lookup: Should never reach this, all envs popped."
        | frame :: frames ->
            match Map.tryFind str frame with
            | Some x -> x
            | None ->
                match frames with
                | [] -> Error $"lookup: Symbol lookup failure - {str}"
                | _ -> lookup_tail frames

    lookup_tail env

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

    | Error _ as error -> (error, env)
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
let atom args env =
    match args with
    | [] -> Error "atom: No arguments given"
    | [ x ] ->
        match x with
        | Sublist _ -> Bool false
        | Error _ as error -> error
        | _ -> Bool true

    | _ -> Error "atom: Too many arguments"

let quote args env =
    match args with
    | [] -> Error "quote: No arguments given"
    | [ x ] -> x
    | _ -> Error "quote: Too many arguments"

let begin' args env = args |> List.rev |> List.head

let car args env =
    match args with
    | [] -> Error "car: No arguments given"
    | [ x ] ->
        match x with
        | Sublist l ->
            match l with
            | [] -> Sublist []
            | x :: xs -> x

        | _ -> Error "car: Not a list"

    | _ -> Error "car: Too many arguments"

let cdr args env =
    match args with
    | [] -> Error "cdr: No arguments given"
    | [ x ] ->
        match x with
        | Sublist l ->
            match l with
            | [] -> Error "cdr: Empty list"
            | x :: xs -> Sublist xs

        | _ -> Error "cdr: Not a list"

    | _ -> Error "cdr: Too many arguments"

let cons (args: Expression list) env =
    match args with
    | [] -> Error "cons: No arguments given"
    | [ _ ] -> Error "cons: Too few arguments given"
    | x :: y :: [] ->
        match y with
        | Sublist l -> Sublist(x :: l)
        | _ -> Error "cons: Not a list"

    | _ -> Error "cons: Too many arguments"

//if it's not defined in the local lambda frame, define it
let define (args: Expression list) env =
    match args with
    | [ Symbol name; value ] ->
        match env with
        | [] -> (Error "define: Should never reach this, all envs popped", env)
        | frame :: frames ->
            match Map.tryFind name frame with
            | Some found -> (Error $"define: Symbol already defined {name}", env)
            | None ->
                let (res_value, res_env) = eval env value
                let (res_frame :: res_frames) = res_env
                let new_frame = Map.add name res_value res_frame
                (Symbol name, new_frame :: res_frames)

    | [] -> (Error "define: No name provided", env)
    | [ x ] -> (Error "define: Too few arguments", env)
    | _ -> (Error "define: Too many arguments", env)

let set' (args : Expression list) (env: Env) =
    match args with
    | [ Symbol name; value ] ->
        let predicate x =
            match Map.tryFind name x with
            | Some _ -> true
            | None -> false

        let found =
            try
                env
                |> List.findIndex predicate
            with :? System.Collections.Generic.KeyNotFoundException ->
                -2

        match found with
        | -2 -> (Error $"set!: Symbol not found {name}", env)
        | frameid ->
                //evaluate with whole frame stack otherwise you don't see local vars
                let (res_value, res_env) = eval env value

                let prev, (frame::after) = res_env |> List.splitAt frameid
                let new_frame = Map.add name res_value frame
                //splice new stack frame in the right spot
                (Symbol name, prev @ [new_frame] @ after)

    | [] -> (Error "set!: No name provided", env)
    | [ x ] -> (Error "set!: Too few arguments", env)
    | _ -> (Error "set!: Too many arguments", env)

let if' (args: Expression list) (env: Env) : Expression * Env =
    match args with
    | []
    | [ _ ]
    | _ :: _ :: [] -> (Error "if: Not enough arguments", env)
    | cond :: conseq :: alt :: [] ->
        let (res, res_env) = eval env cond

        match res with
        | Bool true -> eval res_env conseq
        | Bool false
        | Sublist [] -> eval res_env alt //nil is false
        | _ -> eval res_env conseq //anything not 'false' is true

    | _ -> (Error "if: too many arguments", env)

let list' args env = Sublist args //look for and forward errors?

let lambda args env =
    match args with
    | []
    | [_] -> Error "lambda: Not enough arguments"
    | parameters::[body] ->
        match (parameters, body) with
        | Sublist p, Sublist b ->
            let is_all_symbols =
                p
                |> List.forall (fun x -> match x with Symbol x -> true | _ -> false)

            if is_all_symbols then
                let unwrap_symbol (Symbol x) = x
                let param_strs = List.map unwrap_symbol p
                let diff = List.length env - 1 //how many frames deep is this
                let (closure, old) = List.splitAt diff env

                let f fargs (fenv: Env) =
                    if List.length param_strs = List.length fargs then
                        let new_frame = //create a new frame with bound vars
                            Map (List.zip param_strs fargs)
                        //dont close on caller's state!! ... or maybe you do
                        //you need to get the latest stacks and put them on

                        //get how many frames difference are we
                        let new_framestack = [new_frame] @ closure @ fenv

                        printfn $"{new_framestack}"
                        let (res, res_frames) = eval new_framestack body
                        (res, List.skip (diff+1) res_frames) //pop the frame when done //return original caller's frames??

                    else //do nothing to env
                        (Error "lambda eval: parameter arg mismatch", fenv)

                Function <| foldenv_bind f
            else
                Error "lambda: One or more parameters have invalid names"

        | Sublist [], Sublist _
        | Sublist _ , Sublist []
        | _ -> Error "lambda: arguments or body given incorrectly"
    | _ -> Error "lambda: Too many arguments"

let not' args env =
    match args with
    | [] -> Error "not: Not enough arguments"
    | [ x ] ->
        match x with
        | Bool x -> Bool(not x)
        | Sublist [] -> Bool true
        | Error _ as error -> error
        | _ -> Bool false

    | _ -> Error "not: Too many arguments"

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
        | _ -> Error $"comparison: Incorrect or mismatched type"


    match args with
    | [] -> Error $"comparison: No arguments"
    | l -> List.reduce f args

let equals (args: Expression list) (env: Env) =
    let f x y =
        match (x, y) with
        | (Float x, Float y) -> Bool((=) x y)
        | (Integer x, Integer y) -> Bool((=) x y)
        | (Bool x, Bool y) -> Bool((=) x y)
        | _ -> Error $"equals: Incorrect or mismatched type"


    match args with
    | [] -> Error $"equals: No arguments"
    | l -> List.reduce f args

let not_equals (args: Expression list) (env: Env) =
    match equals args env with
    | Bool x -> Bool(not x)
    | Error _ as error -> error
    | _ -> Error "notequals:"

//convenience function for returning results when operation does nothing to env
let nop_env f args env =
    let res = f args env
    (res, env)

let pure_func f =
    f |> nop_env |> foldenv_bind |> Function

let pure_func' t f =
    f |> t |> nop_env |> foldenv_bind |> Function


let cond (args: Expression list) (env: Env) : Expression * Env =
    let begin_bind = begin' |> nop_env |> foldenv_bind

    let rec cond_tail (l: Expression List) (ienv: Env) : Expression * Env =
        match l with
        | [] -> (Sublist [], ienv)
        | x :: xs ->
            match x with
            | Sublist [] -> Error "cond: Empty list", ienv
            | Sublist (h :: t) ->
                let (condition, res_env) = eval ienv h

                match condition with
                | Bool true -> begin_bind t res_env
                | Bool false
                | Sublist [] -> cond_tail xs ienv
                | _ -> begin_bind t res_env
            //everything except false or nil is true

            | _ -> Error "cond: Clause not provided", ienv

    match args with
    | [] -> Error "cond: no arguments given", env
    | l -> cond_tail l env

//----------- Environment
let global_env =
    [ Map [ ("+", (+) |> pure_func' math)
            ("-", (-) |> pure_func' math)
            ("*", (*) |> pure_func' math)
            ("/", (/) |> pure_func' math)
            ("<", (<) |> pure_func' comparison)
            (">", (>) |> pure_func' comparison)
            ("<=", (<=) |> pure_func' comparison)
            (">=", (>=) |> pure_func' comparison)
            ("=", equals |> pure_func)
            ("!=", not_equals |> pure_func)
            ("if", if' |> Function)
            ("atom?", atom |> pure_func)
            ("begin", begin' |> pure_func)
            ("car", car |> pure_func)
            ("cdr", cdr |> pure_func)
            ("cons", cons |> pure_func)
            ("cond", cond |> Function)
            ("define", define |> Function)
            ("set!", set' |> Function)
            ("list", list' |> pure_func)
            ("not", not' |> pure_func)
            ("lambda", lambda |> nop_env |> Function)
            ("quote", quote |> nop_env |> Function)
            ("q", quote |> nop_env |> Function) ] ]

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
    | i when i < 0 -> Error $"parse: Unexpected closing parenthesis"
    | i when i > 0 -> Error $"parse: Missing {i} closing parentheses"
    | 0 ->
        match exprlist with
        | [] -> Error "parse: malformed input"
        | _ ->
            match List.head exprlist with
            | Sublist y as res -> res
            | _ -> Error "parse: missing opening parens"

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

    printfn $"{List.length new_env} : {res} -> {new_env}"

    repl new_env
