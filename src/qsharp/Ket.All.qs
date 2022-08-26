namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function All(size: Int, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let start = oldColumns;
        let end = start + size - 1;
        let output = [Register(start..end)];

        let oracle = _All_oracle(oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + size, oracle);

        log.Info($"Ket.All::Init --> size: {size}");
        return (universe, output);
    }

    operation _All_oracle(
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.All::oracle --> target:{target}");

        // Just call previous oracle
        previous(all, target);
    }
}