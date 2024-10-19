# Test application of syntactic simplification rules

See `../resourses/syntactic-simplification.k`.

We have to keep `../resourses/syntactic-simplification.haskell.kore`, the K front-end does not yet support the `syntactic` attribute. The Kore encoding of the simplification rule `test1-simplify` is manually modified to contain the attribute `syntactic{}("1")`. `../resourses/syntactic-simplification.haskell.kore` should be removed from Git once the compiler supports this feature.

We also need an LLVM definition, and we are re-using ``../resourses/3934-smt.llvm.kore` as it is checked-in, but any definition with `INT` and `BOOL` would work.
