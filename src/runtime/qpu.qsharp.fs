namespace aleph.runtime.qpu.qsharp

open Microsoft.Quantum.Simulation.Core

open aleph.qsharp
type QUniverse = Universe
type QValue = Value

open aleph.parser.ast.typed
open aleph.runtime.Eval

type Universe(state: QUniverse) =
    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"
    member val State = state

type QsharpContext = {
    allocations: Map<int, IQArray<Register>>
    universe: QUniverse
    evalCtx: EvalContext
}

module Convert =
    let BOOL_REGISTER_SIZE = 1
    let INT_REGISTER_SIZE = 2

    let toQValue = function
        | Bool b -> new QValue((if b then (1,BOOL_REGISTER_SIZE) else (0,BOOL_REGISTER_SIZE)))
        | Int i -> new QValue((i, INT_REGISTER_SIZE))
        | _ -> failwith "not an int/bool"
        
    let toQTuple = function
        | Bool b -> [| (Bool b |> toQValue) |] |> QArray<QValue>
        | Int i -> [| (Int i |> toQValue) |] |> QArray<QValue>
        | Tuple t -> 
            t 
            |> List.map toQValue 
            |> List.toArray
            |> QArray<QValue>
        | _ -> failwith "not a tuple"

    let toQSet = function
        | Set s -> 
            s 
            |> Set.toArray
            |> Array.map toQTuple
            |> QArray<IQArray<QValue>>
        | _ -> failwith "not a set"
        
    let toValue (result: IQArray<QValue>) = 
        let one (v: QValue) =
            if v.size = BOOL_REGISTER_SIZE then
                if v.value = 1 then Bool true else Bool false
            elif v.size = INT_REGISTER_SIZE then
                Int (int v.value)
            else
                failwith "not an int/bool"
        if result.Length = 1 then
            one result.[0]
        else
            Tuple (result |> Seq.map one |> Seq.toList)

type Processor(sim: IOperationFactory) =
    let rec prepare_ket (ket : Ket, ctx: QsharpContext) =
        match ctx.allocations.TryFind ket.Id with
        | Some registers -> 
            let ctx = { ctx with universe = UpdateUniverseOutput.Run(sim, ctx.universe, registers).Result }
            ctx |> Ok
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

    and prepare_literal (values, ctx) =
        eval_classic(values, ctx.evalCtx)
        ==> fun (values, evalCtx) ->
            match values with
            | Value.Set _ ->
                let v = values |> Convert.toQSet
                let u = ket.Literal.Run(sim, v, ctx.universe).Result
                { ctx with universe = u; evalCtx = evalCtx} |> Ok
            | _ -> 
                $"Invalid classic value for a ket literal: {values}" |> Error


    interface QPU with
        member this.Measure (universe: IUniverse) =
            let u = universe :?> Universe
            let sample = Sample.Run(sim, u.State).Result |> Convert.toValue
            sample |> Ok
        
        member this.Prepare (u, evalCtx) =
            assert (evalCtx.qpu = this)

            match u with
            | U.Prepare q ->
                eval_quantum (q, evalCtx)
                ==> fun (ket, evalCtx) ->
                    match ket with
                    | Value.Ket ket ->
                        let ctx = {
                            allocations = Map.empty
                            evalCtx = evalCtx
                            universe = BigBang.Run(sim).Result }
                        prepare_ket (ket, ctx)
                        ==> fun ctx ->
                            (Value.Universe (Universe(ctx.universe)), ctx.evalCtx) |> Ok
                    | _ -> "" |> Error
            | U.Var id ->
                match evalCtx.heap.TryFind id with
                | Some (Value.Universe u) ->
                    (Value.Universe u, evalCtx) |> Ok
                | _ ->
                    $"Invalid variable: {id}. Expecting universe." |> Error
            | U.Block (stmts, body) ->
                eval_stmts (stmts, evalCtx)
                ==> fun evalCtx ->
                    (this :> QPU).Prepare (body, evalCtx)
            | U.CallMethod (method, args) ->
                eval_callmethod(method, args, evalCtx)
