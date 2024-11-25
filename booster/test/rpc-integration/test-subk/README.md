Tests involving K sequences
---------------------------

The [definition `subk.k`](../resources/subk.k) for these tests rewrites `ping` to `pong` and vice-versa, while checking a second configuration cell to not contain a K sequence with the same state at the head.
In addition, the rewrite rules use a partial identity function on states, such that all rewrites will fall back to kore-rpc.

Interestingly, it is forbidden by `kompile` to use `A ~> .K` as a direct child of `==K`. However, functions that produce this expression are apparently fine.

* `sortk-var-branch.execute`: branches on whether the additional cell contains `ping` or not. If it contains `ping`, the state is stuck.
* `sortk-stuck.execute`: the additional cell is initialised with `pong`. Therefore, the rule `pong => ping` cannot be applied, the result is stuck.
* `sortk-equal.simplify`: simplify request for a predicate involving `==K` on arguments of sort `K`.

* `not-subsort.execute`: expects an error saying that `SortK` is not subsort of `SortKItem`.
