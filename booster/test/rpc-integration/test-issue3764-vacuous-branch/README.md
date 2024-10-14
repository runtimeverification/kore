# Regression test for Issue 3674

`kore-rpc` returns a `Branching` result for an EVM `JUMPI` instruction , splitting on the jump condition.

However, the negated jump condition can be simplified to `False` easily, which makes one of the returned branches `#Bottom`. Execution should continue.

The files for this test are a slight reduction of the original input, but the definition has been left intact for further analysis.

* `state-branch-in-zero` 
  - The `k` cell contains the `JUMPI` instruction. Booster aborts instantly on the uncertain condition (containing variables).
  - `kore-rpc` returns `Branching`, splitting on the jump condition
  - The second branch is then pruned (by post-exec simplification), and execution continues for one more step (booster returns `DepthBound`)
* `state-branch-after-one`
  - The `k` cell contains `#gas[_, _] ~> JUMPI ... ~> CONTINUATION`, with the `JUMPI` instruction from before.
  - In one step, the `#gas[_, _]` instruction is removed since `useGas` contains `false`
  - After that, the same as in `state-branch-in-zero` happens.
  - Output is virtually the same, except the `depth` field and one returned `rule-id`.


`booster-dev` currently branches at depth 0 for `state-branch-in-zero` and is unable to proceed. This is worth investigating.
