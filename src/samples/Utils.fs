
module Utils
    open aleph.runtime.Eval
    open Microsoft.Quantum.Simulation.Simulators

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

    let simulate program = program |> run (aleph.runtime.qpu.classic.Processor())

    let execute program = program |> run (aleph.runtime.qpu.qsharp.Processor(new QuantumSimulator()))

