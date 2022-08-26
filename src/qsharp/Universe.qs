namespace aleph.qsharp {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    open aleph.qsharp.grover as grover;
    open aleph.qsharp.log as log;

    function BigBang(): Universe {
        return Universe(0, 1, [], _tracker(_, _));
    }

    function UpdateUniverseOutput(original: Universe, o: Register[]) : Universe {
        return original
            w/ output <- o;
    }
    
    operation Sample(universe: Universe) : Value[] {
        let (_, columns, output, oracle) = universe!;

        use qubits = Qubit[columns]; // One extra for tracker

        Prepare(universe, qubits);

        mutable result = [];
        for r in output {
            let value = ResultArrayAsInt(ForEach(M, qubits[r!]));
            let size = Length(RangeAsIntArray(r!));
            set result += [ Value(value, size) ];
        }

        ResetAll(qubits);
        return result;
    }

    operation _tracker (qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($".tracker.");
        CNOT(qubits[0], target);
    }

    operation Prepare(universe: Universe, qubits: Qubit[]) : Unit {
        let (rows, columns, _, oracle) = universe!;
        let tracker = qubits[0];

        repeat {
            ResetAll(qubits);
            ApplyToEachA(H, qubits);
            grover.Apply(oracle, qubits, rows);
        } until ((M(tracker) == One) or (rows == 0));

        if (log.INFO_ON()) {
            Message($"[Q#] Final state after Prepare: ");
            DumpRegister((), qubits[1..Length(qubits)-1]);
            Message("");
        }
    }
}