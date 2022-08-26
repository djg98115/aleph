namespace aleph.qsharp {

    newtype Value = (
        value: Int,
        size: Int
    );

    newtype Register = Range;

    newtype Universe = (
        rows: Int,
        columns: Int,
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl
    );

}