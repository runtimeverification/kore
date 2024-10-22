# Tests checking the behaviour related to `substitution`s in input and output

The definition contains the following rules that introduce substitutions:

* `concrete-subst => a ... ensures X ==Int 42`
* `symbolic-subst => a ... ensures X ==Int Y`

for a configuration containing cells `int` and `jnt` with `X` and `Y` respectively.

In all tests, `booster-dev` exhibits different behaviour because the final state
is not simplified using `kore-rpc`.

## Substitution as output

* `state-concrete-substitution.execute`
  - starts from `concrete-subst`
  - `kore-rpc-booster` returns the `X ==Int 42` as a `substitution` and applies it to the returned `state`
  - `booster-dev` returns `X ==Int 42` as a `predicate` (and does not apply it)
* `state-symbolic-substitution.execute`
  - starts from `symbolic-subst`
  - `kore-rpc-booster` returns the `X ==Int Y` as a `substitution` and applies it to the returned `state`
  - `booster-dev` returns `X ==Int Y` as a `predicate` (and does not apply it)


## Substitution in input

NB: Booster applies the given substitution before performing any action.

* `state-symbolic-two-substitutions.execute`
  - starts from `symbolic-subst`
  - with an additional constraint `Y = 1 +Int X` (internalised as a substitution)
  - leading to a contradictory constraint `X = 1 +Int X` after
    rewriting and adding `Y =Int X` from the `ensures`-clause
    - `kore-rpc-booster` returns `vacuous` after 1 step
    - `kore-rpc-dev` returns `vacuous` after 0 steps (detects the contradiction earlier)
    - `kore-rpc-dev` reproduces the exact input as `state` while
      `kore-rpc-booster` splits off `substitution` (from input) and `predicate` (from the rule)
* `state-circular-equations.execute`
  - starts from `concrete-subst`
  - with two equations that have circular dependencies: `Y = 1 +Int X`, `X = Y -Int 1`
  - leading to end state with `X == 42` from the `ensures`-clause
  - `booster-dev` return a trivial circular constraint `X ==Int X`
* `state-symbolic-bottom-predicate.execute`
  - starts from `symbolic-subst`
  - with an equation that is instantly false: `X = 1 +Int X`
  - leading to a vacuous state in `kore-rpc-booster` and `booster-dev` at 0 steps,
  - while `kore-rpc-dev` returns `stuck` instantly after 0 steps,
    returning the exact input as `state`.
