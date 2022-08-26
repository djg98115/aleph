namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Microsoft.Quantum.Simulation.Simulators
open Microsoft.Quantum.Simulation.Core

open aleph.qsharp
open aleph.runtime.Eval
open aleph.runtime.qpu.qsharp
open aleph.runtime.qpu.qsharp.Convert

open aleph.tests.Utils


[<TestClass>]
type TestQsharpCode () =
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
            let u = ket.Literal.Run(sim, v, bigbang).Result
            printfn "Universe = %A" u
            Assert.AreEqual(int64(values.Length), u.rows)
            Assert.AreEqual(int64(qubits + 1), u.columns)

            let r = Sample.Run(sim, u).Result |> toValue
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
            ], INT_REGISTER_SIZE
            [
                Tuple [ Int 0; Int 0 ]
                Tuple [ Int 0; Int 1 ]
                Tuple [ Int 0; Int 2 ]
            ], INT_REGISTER_SIZE + INT_REGISTER_SIZE 
            [
                Tuple [ Int 0; Bool false; Int 0 ]
                Tuple [ Int 0; Bool true; Int 1 ]
                Tuple [ Int 0; Bool true; Int 2 ]
                Tuple [ Int 2; Bool true; Int 3 ]
            ], INT_REGISTER_SIZE + BOOL_REGISTER_SIZE + INT_REGISTER_SIZE
        ]
        |> List.iter test_one

    [<TestMethod>]
    member this.TestLiteral () =
        let ctx = this.Context

        [
            // u.Ket [],
            //     []
            // | false >
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
        ]
        |> List.iter (verify_expression ctx)
