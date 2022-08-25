namespace aleph.runtime.qpu.qsharp

open aleph.qsharp

open aleph.parser.ast.typed
open aleph.runtime.Eval
open Microsoft.Quantum.Simulation.Core

type QUniverse = aleph.qsharp.Universe
type QValue = aleph.qsharp.Value


type Universe(state: QUniverse) =
    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"
    member val State = state

type QsharpContext = {
    allocations: Map<int, IQArray<QRange>>
    sim: Simulator
    universe: QUniverse
    evalCtx: EvalContext
}

type Processor() =
    let random = System.Random()

    let rec prepare_ket (ket : Ket, ctx: QsharpContext) =
        
        match ctx.allocations.TryFind ket.Id with
        | Some registers -> 
            UpdateOutput.Run()
            ctx |> Ok        // Already prepared...
        | None ->
            prepare (ket.StatePrep, ctx)

    and prepare (q, ctx) =
        match q with
        | Q.Var _
        | Q.Literal _
        | Q.KetAll _
        | Q.Equals _
        | Q.Add _
        | Q.Multiply _
        | Q.Not _
        | Q.And _
        | Q.Or _
        | Q.Project _
        | Q.Index _
        | Q.Join  _
        | Q.Solve  _
        | Q.Block  _
        | Q.IfQuantum  _
        | Q.IfClassic  _
        | Q.CallMethod  _
        | Q.Summarize _ ->
            $"Not implemented: {q}" |> Error


    interface QPU with
        member this.Measure (universe: IUniverse) =
            "Measure Not implemented" |> Error
        
        member this.Prepare (u, ctx) =
            assert (ctx.qpu = this)
            memory <- { allocations = Map.empty }
            match u with
            | U.Prepare q ->
                eval_quantum (q, ctx)
                ==> fun (ket, ctx) ->
                    match ket with
                    | Value.Ket ket -> 
                        prepare_ket (ket, ctx)
                        ==> fun (universe, ctx) ->
                            (Value.Universe (Universe(universe)), ctx) |> Ok
                    | _ -> "" |> Error
            | U.Var id ->
                match ctx.heap.TryFind id with
                | Some (Value.Universe u) ->
                    (Value.Universe u, ctx) |> Ok
                | _ ->
                    $"Invalid variable: {id}. Expecting universe." |> Error
            | U.Block (stmts, body) ->
                eval_stmts (stmts, ctx)
                ==> fun ctx ->
                    (this :> QPU).Prepare (body, ctx)
            | U.CallMethod (method, args) ->
                eval_callmethod(method, args, ctx)
