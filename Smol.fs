module Smol

//id the closure with Length.list
type Id = Id of int

type Expression =
    | Symbol of string
    | Float of float
    | Integer of int
    | Bool of bool
    | Sublist of List<Expression>
    | Function of Function
    | Error of string

and Function = List<Expression> -> Env -> Expression * Env
and Frame = Map<string, Expression>
and Env = Frame list * Closure list
and Closure = Id * List<Frame>

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

    lookup_tail (fst env)

//----------- eval
let rec eval env expr =
    let rec loop_tail head args lenv =
        match head with
        | Function f -> f args lenv
        | Symbol str ->
            loop_tail (lookup str lenv) args lenv
        | e -> Error $"eval: {e} not callable", lenv

    match expr with
    | Float _
    | Integer _
    | Bool _ as literal -> (literal, env)
    | Symbol str -> (lookup str env, env)
    | Sublist (h :: args) -> loop_tail h args env
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
//implemented separate equality comparison since typesystem couldn't generalize float -> bool
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
        match fst env with
        | [] -> (Error "define: Should never reach this, all envs popped", env)
        | frame :: frames ->
            match Map.tryFind name frame with
            | Some found -> (Error $"define: Symbol already defined {name}", env)
            | None ->
                let (res_value, res_env) = eval env value
                let (res_frame :: res_frames) = fst res_env
                let new_frame = Map.add name res_value res_frame
                (Symbol name, (new_frame :: res_frames, snd res_env))

    | [] -> (Error "define: No name provided", env)
    | [ x ] -> (Error "define: Too few arguments", env)
    | _ -> (Error "define: Too many arguments", env)

let set' (args: Expression list) (env: Env) =
    match args with
    | [ Symbol name; value ] ->
        let predicate x =
            match Map.tryFind name x with
            | Some _ -> true
            | None -> false

        let found =
            try
                (fst env) |> List.findIndex predicate
            with
            | :? System.Collections.Generic.KeyNotFoundException -> -2

        match found with
        | -2 -> (Error $"set!: Symbol not found {name}", env)
        | frameid ->
            //evaluate with whole frame stack otherwise you don't see local vars
            let (res_value, res_env) = eval env value

            let prev, (frame :: after) = fst res_env |> List.splitAt frameid
            let new_frame = Map.add name res_value frame
            //splice new stack frame in the right spot
            (Symbol name, (prev @ [ new_frame ] @ after, snd res_env))

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

//define the anonymous function
let procedure (Id closure_id) param_strs body fargs (fenv: Env) =
    if List.length param_strs = List.length fargs then
        let unwrap_id (Id x) = x
        let id_predicate x = unwrap_id (fst x) = closure_id

        let frame_stack = fst fenv
        let closure_list = snd fenv

        let new_frame = //create a new frame with bound vars
            Map(List.zip param_strs fargs)

        //retrieve the closure
        let closure =
            snd <| List.find id_predicate closure_list

        let closure_num_frames = List.length closure

        //push the frame on top of the closure and then on
        //existing frame_stack
        let new_framestack = [ new_frame ] @ closure @ frame_stack

        //printfn $"id : {closure_id} - frames-global : {List.length (new_framestack)-1}"
        let (res, res_env) = eval (new_framestack, closure_list) body

        //pop the current frame w pattern matching
        let _::res_framestack = fst res_env
        let res_closure_list = snd res_env

        //retrieve the modified closure and outer frames
        let new_closure, new_outer_frames =
            List.splitAt closure_num_frames res_framestack

        //construct the new list of closures
        let new_closure_list =
            [ (Id closure_id, new_closure) ]
            @ List.filter (id_predicate >> not) res_closure_list

        let new_env = (new_outer_frames, new_closure_list)
        (res, new_env)

    else //do nothing to env
        (Error "procedure: parameter arg mismatch", fenv)


//lambda is not a nop_env because it adds closures
let lambda (args: Expression list) (env: Env) =
    match args with
    | []
    | [ _ ] -> Error "lambda: Not enough arguments", env
    | parameters :: [ body ] ->
        match (parameters, body) with
        | Sublist p, Sublist b ->
            let is_all_symbols =
                p
                |> List.forall
                    (fun x ->
                        match x with
                        | Symbol x -> true
                        | _ -> false)

            if is_all_symbols then
                let unwrap_symbol (Symbol x) = x
                let param_strs = List.map unwrap_symbol p
                let current_framestack = fst env
                let current_closure_list = snd env

                //how many frames deep is this rel to global frame
                let diff = List.length current_framestack - 1

                //get the current stack frames to create closure from
                let (closure, old) = List.splitAt diff current_framestack

                //check how many closures there are
                let closure_id = Id(List.length <| current_closure_list)

                //add the closure to the environment and bind procedure
                let env_with_closure =
                    (current_framestack, [ (closure_id, closure) ] @ current_closure_list)

                let f =
                    procedure closure_id param_strs body
                    |> foldenv_bind

                Function f, env_with_closure
            else
                Error "lambda: One or more parameters have invalid names", env

        | Sublist [], Sublist _
        | Sublist _, Sublist []
        | _ -> Error "lambda: arguments or body given incorrectly", env
    | _ -> Error "lambda: Too many arguments", env

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
let init_framestack =
    [ Map [ ("+", (+) |> pure_func' math)
            ("-", (-) |> pure_func' math)
            ("*", (*) |> pure_func' math)
            ("/", (/) |> pure_func' math)
            ("<", (<) |> pure_func' comparison)
            (">", (>) |> pure_func' comparison)
            ("<=", (<=) |> pure_func' comparison)
            (">=", (>=) |> pure_func' comparison)
            ("==", equals |> pure_func)
            ("!=", not_equals |> pure_func)
            ("if", if' |> Function)
            ("atom?", atom |> pure_func)
            ("begin", begin' |> pure_func)
            ("car", car |> pure_func)
            ("cdr", cdr |> pure_func)
            ("cons", cons |> pure_func)
            ("cond", cond |> Function)
            ("define", define |> Function)
            ("def", define |> Function)
            ("set!", set' |> Function)
            ("list", list' |> pure_func)
            ("not", not' |> pure_func)
            ("lambda", lambda |> Function)
            ("\\", lambda |> Function)
            ("quote", quote |> nop_env |> Function)
            ("q", quote |> nop_env |> Function)
            ("'", quote |> nop_env |> Function) ] ]

let init_closure_list: Closure list = []

let global_env = (init_framestack, init_closure_list)

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

    printfn $"{(List.length (fst new_env), List.length (snd new_env))} : {res}
    -> {snd new_env}"

    repl new_env

//------load from file
let get_expr (str: string) =
    let rec get_expr_tail (expr: string) rest i =
        match (expr,i) with
        | ("", Some 0) ->
            get_expr_helper expr rest i
        | (a, Some 0) ->
            expr, rest
        | (a, i) ->
            get_expr_helper expr rest i

    and get_expr_helper (expr: string) rest i =
        match rest with
        | "" -> expr, rest
        | rest ->
            let current_char = rest[0]
            let expr_final = expr + string current_char
            let rest_final = rest[1..]

            match current_char with
            | '(' ->
                let num =
                    match i with
                    | Some num -> Some (num + 1)
                    | None -> Some 1
                get_expr_tail expr_final rest_final num
            | ')' ->
                let num =
                    match i with
                    | Some num -> Some (num - 1)
                    | None -> None //found a ) before any (...
                get_expr_tail expr_final rest_final num
            | _ ->
                get_expr_tail expr_final rest_final i

    get_expr_tail "" str None

let load_and_execute name env =
    let file = System.IO.File.ReadAllText name
    printfn $"File loaded:\n\n{file}\n\n"

    let rec evaluate_file text env =
        let expr, rest_text = get_expr text
        let clean_expr  = //strip newlines
            expr |> String.filter
                (fun x ->
                    match x with
                    | '\r'
                    | '\n' -> false
                    | _ -> true)

        printfn $"Expr: \"{clean_expr}\"\nRest: \"{rest_text}\""
        match (clean_expr, rest_text) with
        | ("", "") -> (Sublist [], env)
        | (_, _) ->
            let (res, new_env) =
                clean_expr
                |> tokenize
                |> parse
                |> eval env

            match (rest_text, res) with
            | ("", _)
            | (_, Error _) ->
                (res, new_env)
            | (_, _) -> //loop
                evaluate_file rest_text new_env

    evaluate_file file env
