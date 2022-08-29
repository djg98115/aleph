[<EntryPoint>]
let main argv =

    let programs = 
        [
            Sandbox.program
            // CoinFlip.program
            // DiceRoll.program
            // DiceRoll.program
            // SolveEquation.program
            // GraphColoring.program
        ]
    
    programs 
    |> List.map Utils.execute
    |> List.sum
