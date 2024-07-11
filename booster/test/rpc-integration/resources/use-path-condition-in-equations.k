module USE-PATH-CONDITION-IN-EQUATIONS
  imports INT
  imports BOOL

  syntax State ::= test1Init()
                 | test1State1()
                 | test1State2()

                 | test2Init()
                 | test2State1()
                 | test2State2()

  syntax Int ::= test1F ( Int ) [function, total, no-evaluators]
               | test2F ( Int ) [function, total, no-evaluators]

  configuration <k> $PGM:State ~> .K </k>
                <int> 0:Int          </int>

  ////////////////////////////////////////////////////////////////////////////////
  // Here the simplification's side condition is syntactically present          //
  // in the path condition and is not checked.                                   //
  // Result: Stuck at depth 2 in state test1State2()                            //
  //         after applying rules test1-init,test1-1-2                          //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test1-init]: <k> test1Init() => test1State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test1-1-2]: <k> test1State1() => test1State2() ... </k>
                    <int> X                                </int>
                    requires test1F(X) >Int 0

  rule [test1F-simplify]: test1F(X:Int) => X requires X ==Int 42 [simplification]

  ////////////////////////////////////////////////////////////////////////////////
  // Here the simplification's side condition is implied by the path condition,  //
  // but we need an SMT solver to establish that.                               //
  // Result: Aborted at depth 1 due to indeterminate condition of rule test2-1-2 //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test2-init]: <k> test2Init() => test2State1() ... </k>
                     <int> _         => ?X                </int>
                     ensures ?X ==Int 42

  rule [test2-1-2]: <k> test2State1() => test2State2() ... </k>
                    <int> X                                </int>
                    requires test2F(X) >Int 0

  rule [test2F-simplify]: test2F(X:Int) => X requires X >Int 0 [simplification]

// to produce input state:
// krun --output kore --depth 1 -cPGM='test1Init()' | kore-parser test-kompiled/definition.kore --module TEST --pattern /dev/stdin --print-pattern-json > state-test1Init.json
// then edit state-test1Init.json to substitute test1State1() for test1Init()

endmodule
