namespace aleph.runtime.qpu.qsharp

open Microsoft.Quantum.Simulation.Core

open aleph.qsharp
type QUniverse = Universe
type QValue = Value
type QRegister = Register
type QRegisters = IQArray<QRegister>

open aleph.parser.ast.typed
open aleph.runtime.Eval


type QsharpContext = {
    allocations: Map<int, QRegisters>
    universe: QUniverse
    evalCtx: EvalContext
}

module Convert =
    let BOOL_REGISTER_SIZE = 1
    let INT_REGISTER_DEFAULT_SIZE = 2

    let toQValue = function
        | Bool b -> new QValue((if b then (1,BOOL_REGISTER_SIZE) else (0,BOOL_REGISTER_SIZE)))
        | Int i -> new QValue((i, INT_REGISTER_DEFAULT_SIZE))
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
            else
                Int (int v.value)
        if result.Length = 1 then
            one result.[0]
        else
            Tuple (result |> Seq.map one |> Seq.toList)

type Universe(sim: IOperationFactory, state: QUniverse, registers: QRegisters) =
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"

    member this.Sample() =
        match value with
        | Some v ->
            v
        | None ->
            let sample = Sample.Run(sim, state, registers).Result |> Convert.toValue
            value <- Some sample
            sample

type Processor(sim: IOperationFactory) =
    let rec prepare_ket (ket : Ket, ctx: QsharpContext) =
        match ctx.allocations.TryFind ket.Id with
        | Some registers -> 
            (registers, ctx) |> Ok
        | None ->
            prepare (ket.StatePrep, ctx)

    and prepare (q, ctx) =
        match q with
        | Q.Var id -> prepare_var(id, ctx)
        | Q.Literal values -> prepare_literal (values, ctx)
        | Q.KetAll size -> prepare_ketall (size, ctx)

        | Q.Join (left, right) -> prepare_join(left, right, ctx)
        
        | Q.Equals _
        | Q.Add _
        | Q.Multiply _
        | Q.Not _
        | Q.And _
        | Q.Or _
        | Q.Project _
        | Q.Index _
        | Q.Solve  _
        | Q.Block  _
        | Q.IfQuantum  _
        | Q.IfClassic  _
        | Q.CallMethod  _
        | Q.Summarize _ ->
            $"Not implemented: {q}" |> Error

    and prepare_var (id, ctx) =
        match ctx.evalCtx.heap.TryFind id with
        | Some (Value.Ket ket) ->
            prepare_ket (ket, ctx)
            ==> fun (registers, ctx) ->
                (registers, ctx) |> Ok
        | _ ->
            $"Invalid variable: {id}. Expecting ket." |> Error

    and prepare_literal (values, ctx) =
        eval_classic(values, ctx.evalCtx)
        ==> fun (values, evalCtx) ->
            match values with
            | Value.Set w when w.IsEmpty->
                (new QArray<Register>() :> QRegisters, { ctx with evalCtx = evalCtx }) |> Ok
            | Value.Set _ ->
                ket.Literal.Run(sim, values |> Convert.toQSet, ctx.universe).Result
                |> qsharp_result { ctx with evalCtx = evalCtx }
            | _ -> 
                $"Invalid classic value for a ket literal: {values}" |> Error

    and prepare_ketall (size, ctx) =
        eval_classic (size, ctx.evalCtx)
        ==> fun (size, evalCtx) ->
            match size with
            | Value.Int i ->
                ket.All.Run(sim, i |> int64, ctx.universe).Result
                |> qsharp_result { ctx with evalCtx = evalCtx }
            | _ -> 
                $"Invalid ket_all size, expected int got: {size}" |> Error

    and prepare_join (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                (QArray.Add(left, right) :> QRegisters, ctx) |> Ok

    and qsharp_result ctx value =
        let struct (u, r) = value
        (r, { ctx with universe = u }) |> Ok



    interface QPU with
    
        member this.Measure (universe: IUniverse) =
            let u = universe :?> Universe
            u.Sample() |> Ok
        
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
                        ==> fun (registers, ctx) ->
                            (Value.Universe (Universe(sim, ctx.universe, registers)), ctx.evalCtx) |> Ok
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
