# Tests for `kore-rpc-booster` returning vacuous results after simplification

Since `kore-rpc-booster` does not consider combinations of constraints, it won't discover when a reached state (or the initial state) simplifies to `\bottom`. In such cases, the `execute` request should return the current state with `"reason": "vacuous"` in the result.

The `diamond/test.k` semantics is used, which consists of simple state
transition rules featuring additional constraints on a variable
`X:Int` in the configuration.

Rules `init` and `AC` introduce constraints on this variable:

* Rule `init => a` ensures `X ==Int 42`
* Rule `a => c` ensures `X =/=Int 42`

1) _vacuous-at-branch_

   _Input:_
   `execute` request with initial state `<k>init</k><int>0</int>`

   _Expected:_
   - Rewrite with `init` rule creates a false constraint `0 ==Int 42`
     because `X ==Int 0` initially.
   - The rewrite proceeds to the branch (`a => b` or `a => c`)
   - Both branches are discarded because of the contradiction, the
     result is `vacuous` with state `a`.

   With `kore-rpc-dev`, the term won't be rewritten because the contradiction
   will be recognised during the rewrite (simplification after each step).

1) _vacuous-var-at-branch_ Same as 1) but using a variable `N` for the
   `int` field and a constraint `N ==Int 0`.

1) _vacuous-without-rewrite_

   _Input:_
   - `execute` request with initial state  `<k>d</k><int>N</int> \and N
     ==Int 1  \and N =/=Int  1` (A contradiction in the initial constraints).

   _Expected:_
   - The rewrite is stuck with `<k>d</k><int>N</int> \and...(contradiction)`
   - The result is simplified and discovered to be `vacuous` (with state `d`).
1) _vacuous-not-rewritten_

   _Input:_
   - `execute` request with initial state  `<k>b</k><int>N</int> \and N
     ==Int 1  \and N =/=Int  1` (A contradiction in the initial constraints).

   _Expected:_
   - The state is simplified and discovered to be `vacuous` (with state `b`).

With `kore-rpc-dev`, some contradictions will be discovered before or while
attempting to rewrite (at the time of writing, it returns `stuck`, though).
