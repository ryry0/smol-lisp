[<EntryPoint>]
let main args =
    match args with
    | [||] -> Smol.repl Smol.global_env
    | [| name |] ->
        let res, res_env =
            Smol.load_and_execute name Smol.global_env

        match res with
        | Smol.Error e -> printfn $"load and execute:  {e}"
        | _ -> Smol.repl res_env

    0
