module USE-PATH-CONDITION-IN-EQUATIONS
  imports INT
  imports BOOL

  syntax State ::= test1Init()
                 | test1State1()
                 | test1State2()

                 | test2Init()
                 | test2State1()
                 | test2State2()

                 | test3Init()
                 | test3State1()
                 | test3State2()

                 | test4Init()
                 | test4State1()
                 | test4State2()
                 | test4State3()

  syntax Int ::= test1F ( Int ) [function, total, no-evaluators]
               | test2F ( Int ) [function, total, no-evaluators]
               | test3F ( Int ) [function, total]
               | test4F ( Int ) [function, total]

  configuration <k> $PGM:State ~> .K </k>
                <int> 0:Int          </int>

  ////////////////////////////////////////////////////////////////////////////////
  // Here the simplification's side condition is syntactically present          //
  // in the path condition and is not checked.                                  //
  // Result kore-rpc-booster and booster-dev:                                   //
  //   Stuck at depth 2 in state test1State2()                                  //
  //   after applying rules test1-init,test1-1-2                                //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test1-init]: <k> test1Init() => test1State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test1-1-2]: <k> test1State1() => test1State2() ... </k>
                    <int> X                                </int>
                    requires test1F(X) >Int 0

  rule [test1F-simplify]: test1F(X:Int) => X requires X ==Int 42 [simplification]

  ////////////////////////////////////////////////////////////////////////////////
  // Here the simplification's side condition is implied by the path condition. //
  // Result kore-rpc-booster and booster-dev:                                   //
  //   Stuck at depth 2 in state test2State2(),                                 //
  //   after applying rules test2-init, test2-1-2.                              //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test2-init]: <k> test2Init() => test2State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test2-1-2]: <k> test2State1() => test2State2() ... </k>
                    <int> X                                </int>
                    requires test2F(X) >Int 0

  rule [test2F-simplify]: test2F(X:Int) => X requires X >Int 0 [simplification]

  /////////////////////////////////////////////////////////////////////////////////
  // Exactly like test2, but the function now has actual evaluators, rather than //
  // a simplification-based semantics. Using the SMT solver Booster determines   //
  // that the condition of rule test3-1-2 is False.                              //
  // Result kore-rpc-booster and booster-dev:                                    //
  //   Stuck at depth 1 in state test3State1()                                   //
  //   after applying rules test3-init          .                                //
  /////////////////////////////////////////////////////////////////////////////////
  rule [test3-init]: <k> test3Init() => test3State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test3-1-2]: <k> test3State1() => test3State2() ... </k>
                    <int> X                                </int>
                    requires test3F(X) >Int 0

  rule [test3F-zero-if-x-positive]:       test3F(X:Int) => 0 requires X >Int 0
  rule [test3F-one-if-not-x-nonpositive]: test3F(X:Int) => 1 requires X <=Int 0

  /////////////////////////////////////////////////////////////////////////////////
  // Similar to test3, but now there are two rules. Using the solver, Booster    //
  // determines that the condition of rule test4-1-2 is False.                   //
  // Result kore-rpc-booster and booster-dev:                                    //
  //   Stuck at depth 2 in state test2State3()                                   //
  /////////////////////////////////////////////////////////////////////////////////
  rule [test4-init]: <k> test4Init() => test4State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test4-1-2]: <k> test4State1() => test4State2() ... </k>
                    <int> X                                </int>
                    requires test4F(X) >Int 0

  rule [test4-1-3]: <k> test4State1() => test4State3() ... </k>
                    <int> X                                </int>
                    requires test4F(X) <=Int 0


  rule [test4F-zero-if-x-positive]:       test4F(X:Int) => 0 requires X >Int 0
  rule [test4F-one-if-not-x-nonpositive]: test4F(X:Int) => 1 requires X <=Int 0

// to produce input state:
// krun --output kore --depth 1 -cPGM='test1Init()' | kore-parser test-kompiled/definition.kore --module TEST --pattern /dev/stdin --print-pattern-json > state-test1Init.json
// then edit state-test1Init.json to substitute test1State1() for test1Init()

endmodule
