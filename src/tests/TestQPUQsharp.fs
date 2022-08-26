namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Microsoft.Quantum.Simulation.Simulators
open Microsoft.Quantum.Simulation.Core

open aleph.parser.ast

open aleph.qsharp
open aleph.runtime.Eval
open aleph.runtime.qpu.qsharp
open aleph.runtime.qpu.qsharp.Convert

open aleph.tests.Utils


[<TestClass>]
type TestQPUQsharp () =
    member this.Context = { 
        ClassicValueContext.ctx
        with qpu = aleph.runtime.qpu.qsharp.Processor(new QuantumSimulator())
    }

    [<TestMethod>]
    member this.TestRawLiteral () =
        let sim = new QuantumSimulator()

        let test_one (values: Value list, qubits: int) = 
            let bigbang = BigBang.Run(sim).Result
            let v = Set (new Set<Value>(values)) |> toQSet
            let struct (u, o) = ket.Literal.Run(sim, v, bigbang).Result
            printfn "Universe = %A" u
            Assert.AreEqual(int64(values.Length), u.rows)
            Assert.AreEqual(int64(qubits + 1), u.columns)

            let r = Sample.Run(sim, u, o).Result |> toValue
            printfn "result = %A" r
            Assert.IsTrue(is_valid_answer values r)

        [ 
            [
                Bool true
            ], BOOL_REGISTER_SIZE
            [
                Bool true
                Bool false
            ], BOOL_REGISTER_SIZE
            [
                Int 0
                Int 1
                Int 2
            ], INT_REGISTER_DEFAULT_SIZE
            [
                Tuple [ Int 0; Int 0 ]
                Tuple [ Int 0; Int 1 ]
                Tuple [ Int 0; Int 2 ]
            ], INT_REGISTER_DEFAULT_SIZE + INT_REGISTER_DEFAULT_SIZE 
            [
                Tuple [ Int 0; Bool false; Int 0 ]
                Tuple [ Int 0; Bool true; Int 1 ]
                Tuple [ Int 0; Bool true; Int 2 ]
                Tuple [ Int 2; Bool true; Int 3 ]
            ], INT_REGISTER_DEFAULT_SIZE + BOOL_REGISTER_SIZE + INT_REGISTER_DEFAULT_SIZE
        ]
        |> List.iter test_one

    [<TestMethod>]
    member this.TestLiteral () =
        let ctx = 
            this.Context
            |> add_to_context "k" (AnyType.QType (QType.Ket [Type.Int; Type.Bool])) (u.Ket [
                u.Tuple [ u.Int 0; u.Bool true]
                u.Tuple [ u.Int 0; u.Bool false]
                u.Tuple [ u.Int 1; u.Bool true]
            ])

        [
            // |>
            u.Ket [],
                []
            //| false >
            u.Ket [
                u.Bool false
            ], [
                Bool false
            ]
            // | 1; 2; 3 >
            u.Ket [
                u.Int 1
                u.Int 2
                u.Int 3
            ], [
                Int 1
                Int 2
                Int 3
            ]
            // | (false, false), (false, true) >
            u.Ket [
                u.Tuple [ u.Bool false; u.Bool false ]
                u.Tuple [ u.Bool false; u.Bool true ]
            ], [
                Tuple [ Bool false; Bool false ]
                Tuple [ Bool false; Bool true ]
            ]
            // | (0, false, 0), (0, true, 1), (0, true, 2), (0, true, 3) >
            u.Ket [
                u.Tuple [ u.Int 0; u.Bool false; u.Int 0 ]
                u.Tuple [ u.Int 0; u.Bool true; u.Int 1 ]
                u.Tuple [ u.Int 0; u.Bool true; u.Int 2 ]
                u.Tuple [ u.Int 2; u.Bool true; u.Int 3 ]
            ], [
                Tuple [ Int 0; Bool false; Int 0 ]
                Tuple [ Int 0; Bool true; Int 1 ]
                Tuple [ Int 0; Bool true; Int 2 ]
                Tuple [ Int 2; Bool true; Int 3 ]
            ]
            // |@,4>
            u.KetAll (u.Int 4),
            seq { 0..15 } |> Seq.toList |> List.map Int
            // k
            u.Var "k",
            [
                Tuple [ Int 0; Bool true]
                Tuple [ Int 0; Bool false]
                Tuple [ Int 1; Bool true]
            ]
            // k.0
            u.Project (u.Var "k", u.Int 0),
            [
                Int 0
                Int 1
            ]
            // k.1
            u.Project (u.Var "k", u.Int 1),
            [
                Bool true
                Bool false
            ]
        ]
        |> List.iter (verify_expression ctx)


    [<TestMethod>]
    member this.TestJoinLiterals () =
        let ctx = 
            this.Context 
            |> add_to_context "k" (AnyType.QType (QType.Ket [Type.Int; Type.Bool])) (u.Ket [
                u.Tuple [ u.Int 0; u.Bool true]
                u.Tuple [ u.Int 0; u.Bool false]
                u.Tuple [ u.Int 1; u.Bool true]
            ])

        [
            u.Join(u.Ket [],u.Ket []),
                []
            // (| false >, | true> )
            u.Join (u.Ket [u.Bool false], u.Ket [u.Bool true]),
            [
                Tuple [ Bool false; Bool true ]
            ]
            // Join (| 1; 2 >, | true >)
            u.Join (u.Ket [u.Int 1; u.Int 2], u.Ket [u.Bool true]),
            [
                Tuple [ Int 1; Bool true ]
                Tuple [ Int 2; Bool true ]
            ]
            // Join( | (false, false), (false, true) >, |1, 3> )
            u.Join (
                u.Ket [
                    u.Tuple [ u.Bool false; u.Bool false ]
                    u.Tuple [ u.Bool false; u.Bool true ]],
                u.Ket [ u.Int 1; u.Int 3]),
            [
                Tuple [ Bool false; Bool false; Int 1 ]
                Tuple [ Bool false; Bool true; Int 1 ]
                Tuple [ Bool false; Bool false; Int 3 ]
                Tuple [ Bool false; Bool true; Int 3 ]
            ]
            // ( |@,3>, |1,3> )
            u.Join(u.KetAll (u.Int 3), u.Ket [u.Int 1; u.Int 3]),
            [
                Tuple [ Int 0; Int 1 ]
                Tuple [ Int 0; Int 3 ]
                Tuple [ Int 1; Int 1 ]
                Tuple [ Int 1; Int 3 ]
                Tuple [ Int 2; Int 1 ]
                Tuple [ Int 2; Int 3 ]
                Tuple [ Int 3; Int 1 ]
                Tuple [ Int 3; Int 3 ]
                Tuple [ Int 4; Int 1 ]
                Tuple [ Int 4; Int 3 ]
                Tuple [ Int 5; Int 1 ]
                Tuple [ Int 5; Int 3 ]
                Tuple [ Int 6; Int 1 ]
                Tuple [ Int 6; Int 3 ]
                Tuple [ Int 7; Int 1 ]
                Tuple [ Int 7; Int 3 ]
            ]
            u.Join(u.Var "k", u.Var "k"),
            [
                Tuple [ Int 0; Bool true; Int 0; Bool true ]
                Tuple [ Int 0; Bool false; Int 0; Bool false ]
                Tuple [ Int 1; Bool true; Int 1; Bool true ]
            ]
            // ( |@,4>, |1, 3> ).[3 - 3]
            u.Project(u.Join(u.KetAll (u.Int 3), u.Ket [u.Int 1; u.Int 3]), u.Add (u.Int 3, u.Int -3)),
            seq { 0..15 } |> Seq.toList |> List.map Int
            // ( |@,4>, |1, 3> ).[0 + 1]
            u.Project(u.Join(u.KetAll (u.Int 3), u.Ket [u.Int 1; u.Int 3]), u.Add (u.Int 0, u.Int 1)),
            [ 
                Int 1
                Int 3
            ]
        ]
        |> List.iter (verify_expression ctx)
