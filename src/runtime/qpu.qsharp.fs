namespace aleph.runtime.qpu.qsharp

open aleph.qsharp

open aleph.parser.ast.typed
open aleph.runtime.Eval

type QUniverse = aleph.qsharp.Universe
type QValue = aleph.qsharp.Value

type Memory = {
    allocations: Map<int, QUniverse>
}

type Universe(state: QUniverse) =
    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"
    member val State = state with get,set

type Processor() =
    let random = System.Random()

    let mutable memory = { allocations = Map.empty }

    let rec prepare_ket (ket : Ket, ctx: ValueContext) =
        match memory.allocations.TryFind ket.Id with
        | Some columns -> 
            (columns, ctx) |> Ok        // Already prepared...
        | None ->
            prepare (ket.StatePrep, ctx)
            ==> fun (columns, ctx) ->
                // Assign to the ket the columns returned by the preparation:
                memory <- { memory with allocations = memory.allocations.Add (ket.Id, columns) }
                (columns, ctx) |> Ok

    (*
        Prepares the Universe for the given expression, and returns the index of the
        columns corresponding to the return value of the expression.
     *)
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
