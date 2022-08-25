
module Utils
    open aleph.runtime.Eval
    open aleph.runtime.qpu.classic

    let run qpu program =
        let context = { 
            heap = Map.empty
            typeCtx = Map.empty
            qpu =  qpu
        }

        match run (program, context) with
        | Ok (v, _) ->  printfn $"\nresult: {v}"
        | Error msg -> printfn $"\n!! Failed: {msg} !!"

        printfn ""
        printfn "ℵ:aleph (v0.3)"
        0

    let simulate program = program |> run (Processor())

