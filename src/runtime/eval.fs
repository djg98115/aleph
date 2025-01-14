namespace aleph.runtime

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker

module Eval =

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    type Ket =
        { Id : int
          StatePrep: Q }

    type IUniverse = 
        interface
        inherit System.IComparable
        end

    type Value =
        | Bool of bool
        | Int of int
        | Tuple of Value list
        | Set of Set<Value>
        | Method of Id list * E
        | Ket of Ket
        | Universe of IUniverse

        static member (+) (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int (l + r)
            | _ -> failwith "+ only supported for ints, got {l} + {r}"

        static member (*) (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int (l * r)
            | _ -> failwith "+ only supported for ints, got {l} * {r}"

        static member (==) (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Bool (l = r)
            | _ -> failwith "= only supported for ints, got {l} == {r}"

        static member LessThan (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Bool (l < r)
            | _ -> failwith "< only supported for ints, got {l} == {r}"

        static member Not (l : Value) =
            match l with
            | Value.Bool b -> Value.Bool (not b)
            | _ -> failwith "not only supported for bool values, got {l}"

        static member And (l : Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool (l && r)
            | _ -> failwith "= only supported for bool values, got {l} && {r}"

        static member Or (l : Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool (l || r)
            | _ -> failwith "= only supported for bool values, got {l} || {r}"
            
    type QPU =
        abstract Prepare: U * ValueContext -> Result<Value * ValueContext, string>
        abstract Measure: IUniverse -> Result<Value, string>

    and ValueContext = {
        heap: Map<Id, Value>
        qpu: QPU
        types: TypeContext
    }

    let mutable max_ket = 0

    let rec run (program: Expression, ctx) =
        typecheck (program, ctx.types)
        ==> fun (e, types') ->
            let ctx = { ctx  with types = types' }
            eval (e, ctx)

    and eval (e, ctx) =
        match e with
        | E.Quantum (q, QType.Ket _) ->
            eval_quantum(q, ctx)
        | E.Classic (c, _) ->
            eval_classic (c, ctx)
        | E.Universe (u, _) ->
            ctx.qpu.Prepare (u, ctx)

    and eval_quantum( q, ctx) =
        max_ket <- max_ket + 1
        (Value.Ket {Id= max_ket; StatePrep = q}, ctx) |> Ok

    and eval_classic (c, ctx) =
        match c with
        | C.Var id -> eval_var (id, ctx)

        | C.BoolLiteral b -> eval_bool (b, ctx) 
        | C.IntLiteral i -> eval_int (i, ctx)
        | C.Method (args, body) -> eval_method (args, body, ctx)
        | C.Tuple values -> eval_tuple (values, ctx)
        | C.Set values -> eval_set (values, ctx)
        | C.Range (start, stop) -> eval_range (start, stop, ctx)

        | C.Add (left, right) -> eval_add (left, right, ctx)
        | C.Multiply (left, right) -> eval_multiply (left, right, ctx)
        | C.Equals (left, right) -> eval_equals (left, right, ctx)
        | C.LessThan (left, right) -> eval_lessthan (left, right, ctx)
        | C.And (left, right) -> eval_and (left, right, ctx)
        | C.Or (left, right) -> eval_or (left, right, ctx)
        | C.Not e -> eval_not (e, ctx)

        | C.Project (value, index) -> eval_project (value, index, ctx)
        | C.Index (value, index) -> eval_index (value, index, ctx)
        | C.Join (left, right) -> eval_join (left, right, ctx)

        | C.If (cond, t, e) -> eval_if (cond, t, e, ctx)
        | C.Block (stmts, value) -> eval_block (stmts, value, ctx)
        
        | C.Sample q -> eval_sample (q, ctx)

        | C.CallMethod (method, args) -> eval_callmethod(method, args, ctx)
        
        | C.Summarize _ ->
            $"Not implemented: {c}" |> Error

    and eval_var (id, ctx) =
        match ctx.heap.TryFind id with
        | Some value ->
            (value, ctx) |> Ok
        | _ ->
            $"Variable not found: {id}" |> Error

    and eval_bool (b, ctx) =
        (Value.Bool b, ctx) |> Ok

    and eval_int (i, ctx) =
        (Value.Int i, ctx) |> Ok

    and eval_method (args, body, ctx) =
        (Value.Method (args, body), ctx) |> Ok
        
    and eval_tuple (values, ctx) =
        eval_expression_list (values, ctx)
        ==> fun (values, ctx) -> (Tuple values, ctx) |> Ok

    and eval_set (values, ctx) =
        eval_expression_list (values, ctx)
        ==> fun (values, ctx) -> (Set (Set.ofList values), ctx) |> Ok

    and eval_range (start, stop, ctx) =
        eval_classic (start, ctx)
        ==> fun (start, ctx) ->
            eval_classic (stop, ctx) 
            ==> fun (stop, ctx) ->
                match (start, stop) with
                | Value.Int start, Value.Int stop ->
                    let values = seq { start .. stop - 1 } |> Seq.map Value.Int
                    (Set (Set.ofSeq values), ctx) |> Ok
                | _ -> 
                    $"Range start..stop must be int, got: {start}..{stop}" |> Error

    and eval_add (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (left + right, ctx) |> Ok

    and eval_multiply (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (left * right, ctx) |> Ok

    and eval_equals (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (left == right, ctx) |> Ok

    and eval_lessthan (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (Value.LessThan (left, right), ctx) |> Ok

    and eval_and (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (Value.And (left, right), ctx) |> Ok

    and eval_or (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                (Value.Or (left, right), ctx) |> Ok

    and eval_not (e, ctx) =
        eval_classic (e, ctx)
        ==> fun (e, ctx) ->
            (Value.Not e, ctx) |> Ok

    and eval_project (value, i, ctx) =
        eval_classic (value, ctx)
        ==> fun (value, ctx) ->
            match value with
            | Value.Tuple t ->
                (t.[i], ctx) |> Ok
            | _ ->
                $"project only avaiable for tuples, got: {value}" |> Error

    and eval_index (value, i, ctx) =
        eval_classic (value, ctx)
        ==> fun (value, ctx) ->
            eval_classic (i, ctx) 
            ==> fun (i, ctx) ->
                match (value, i) with
                | Value.Tuple t, Value.Int i ->
                    (t.[i], ctx) |> Ok
                | _ ->
                    $"project only avaiable for tuples and int index, got: {value}[{i}]" |> Error

    and eval_join (left, right, ctx) =
        eval_classic (left, ctx)
        ==> fun (left, ctx) ->
            eval_classic (right, ctx) 
            ==> fun (right, ctx) ->
                match (left, right) with
                | Value.Tuple l, Value.Tuple r ->
                    (Value.Tuple (l @ r), ctx) |> Ok
                | _ ->
                    $"Join only avaiable for tuples, got: {left}, {right}" |> Error

    and eval_if (cond, then_e, else_e, ctx) =
        eval_classic (cond, ctx)
        ==> fun (cond, ctx) ->
            match cond with
            | Value.Bool true ->
                eval_classic(then_e, ctx)
            | Value.Bool false ->
                eval_classic(else_e, ctx)
            | _ ->
                $"if condition must be a boolean expression, got: {cond}" |> Error

    and eval_block (stmts,value, ctx) =
        eval_stmts (stmts, ctx) 
        ==> fun (ctx) -> eval_classic (value, ctx)
        
    and eval_sample (u, ctx) =
        let qpu = ctx.qpu
        qpu.Prepare (u, ctx)
        ==> fun (u, ctx) ->
            match u with 
            | Value.Universe u -> qpu.Measure u ==> fun (v) -> (v, ctx) |> Ok
            | _ -> $"Expecting Prepare to return Universe, got {u}" |> Error

    and eval_callmethod(method, args, ctx) =
        let add_argument ctx' (id, value) =
            ctx' ==> fun ctx' ->
                eval(value, ctx')
                ==> fun (value, ctx') ->
                    { ctx' with heap = ctx'.heap.Add (id, value) } |> Ok
        eval_classic (method, ctx)
        ==> fun (method, ctx) ->
            match method with
            | Value.Method (ids, body) ->
                args
                |> List.zip ids
                |> List.fold add_argument (ctx |> Ok)
                ==> fun ctx' ->
                    eval (body, ctx')
            | _ ->
                $"Expecting method, got {method}" |> Error

    and eval_expression_list (values, ctx) =
        let rec next (items, ctx: ValueContext) =
            match items with
            | head :: tail ->
                eval_classic (head, ctx)
                ==> fun (head, ctx) ->
                    next (tail, ctx)
                    ==> fun (tail, ctx) ->
                        (head :: tail, ctx) |> Ok
            | [] -> ([], ctx) |> Ok
        next (values, ctx)

    and eval_stmts (stmts, ctx) =
        let eval_one ctx' stmt =
            ctx'
            ==> fun(ctx') -> 
                match stmt with 
                | Let (id, e) ->
                    eval (e, ctx')
                    ==> fun (value, ctx') ->
                        { ctx' with heap = ctx'.heap.Add (id, value) } |> Ok
                | Print (msg, expressions) ->
                    printfn "%s" msg
                    let print_one ctx' e =
                        ctx' ==> fun (ctx') -> 
                        eval (e, ctx') 
                        ==> fun (value, ctx') ->
                            printfn "%A" value
                            ctx' |> Ok
                    expressions
                    |> List.fold print_one (ctx' |> Ok)
        stmts
        |> List.fold eval_one (ctx |> Ok)
