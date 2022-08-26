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

    [<TestMethod>]
    member this.TestLiteral () =
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
