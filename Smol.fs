module Smol

//let repl

let tokenize (s : string) =
    s.Replace("(", " ( ").Replace(")", " ) ").Split(' ') |> Array.toList |>
    List.filter (fun x -> x <> "")
