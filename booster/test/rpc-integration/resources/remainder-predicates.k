module REMAINDER-PREDICATES
  imports INT
  imports BOOL

  syntax State ::= test1Init()
                 | test1State1()
                 | test1State2()
                 | test1State3()

                 | test2Init()
                 | test2State1()
                 | test2State2()
                 | test2State3()

                 | test3Init()
                 | test3State1()
                 | test3State2()
                 | test3State3()

                 | test4Init()
                 | test4State1()
                 | test4State2()
                 | test4State3()
                 | test4State4()

                 | test5Init()
                 | test5State1()
                 | test5State2()
                 | test5State3()
                 | test5State4()
                 | test5State5()

                 | test6Init()
                 | test6State1()
                 | test6State2()
                 | test6State3()
                 | test6State4()
                 | test6State5()

                 | test11Init()
                 | test11State1()
                 | test11State2()
                 | test11State3()

  configuration <k> $PGM:State ~> .K </k>
                <int> 0:Int          </int>

  ////////////////////////////////////////////////////////////////////////////////
  /// two rules apply with UNSAT remainder predicate, no further rules apply.   //
  /// Results in 2 branches.                                                    //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test1-init]: <k> test1Init() => test1State1() ... </k>
                     <int> _         => ?_X               </int>

  rule [test1-1-2]: <k> test1State1() => test1State2() ... </k>
                    <int> X                                </int>
                    requires X ==Int 0

  rule [test1-1-3]: <k> test1State1() => test1State3() ... </k>
                    <int> X                                </int>
                    requires X =/=Int 0

  ////////////////////////////////////////////////////////////////////////////////
  /// two rules apply with UNSAT remainder predicate,                           //
  /// but only after function evaluation. No further rules apply.               //
  /// Results in 2 branches.                                                    //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test11-init]: <k> test11Init() => test11State1() ... </k>
                      <int> _         => ?_X               </int>

  rule [test11-1-2]: <k> test11State1() => test11State2() ... </k>
                     <int> X                                </int>
                     requires test11bool2Word(X ==Int 0) ==Int 1

  rule [test11-1-3]: <k> test11State1() => test11State3() ... </k>
                     <int> X                                </int>
                     requires test11bool2Word(X =/=Int 0) ==Int 1

  syntax Int ::= test11bool2Word ( Bool ) [symbol(bool2Word), function, total, injective, smtlib(bool2Word)]

  rule [test11-b2w-eq-zero]: test11bool2Word(B) ==Int 0 => notBool B [simplification(30), comm]
  rule [test11-b2w-eq-one]:  test11bool2Word(B) ==Int 1 => B         [simplification(30), comm]

  ////////////////////////////////////////////////////////////////////////////////
  /// two rules apply with SAT remainder predicate,                             //
  /// have to consider the remainder branch where X ==Int 0,                    //
  /// no further rules apply.                                                   //
  /// Aborts with boosted-dev; results in 2 branches in kore-rpc-booster.       //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test2-init]: <k> test2Init() => test2State1() ... </k>
                     <int> _         => ?_X               </int>

  rule [test2-1-2]: <k> test2State1() => test2State2() ... </k>
                    <int> X                                </int>
                    requires X >Int 0

  rule [test2-1-3]: <k> test2State1() => test2State3() ... </k>
                    <int> X                                </int>
                    requires X <Int 0

  ////////////////////////////////////////////////////////////////////////////////
  /// two rules apply with SAT remainder predicate,                             //
  /// have to consider the remainder branch where X ==Int 0,                    //
  /// but the are no further rules to apply.                                    //
  /// Aborts with boosted-dev; results in 2 branches in kore-rpc-booster.       //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test3-init]: <k> test3Init() => test3State1() ... </k>
                     <int> _         => ?_X               </int>

  rule [test3-1-2]: <k> test3State1() => test3State2() ... </k>
                    <int> X                                </int>
                    requires X >Int 0

  rule [test3-1-3]: <k> test3State1() => test3State3() ... </k>
                    <int> X                                </int>
                    requires X <Int 0

  ////////////////////////////////////////////////////////////////////////////////
  /// two hight-priorty rules apply with SAT remainder predicate,               //
  /// have to consider the remainder branch where X ==Int 0,                    //
  /// one further regular rule applies,                                         //
  /// whose requires clause explicitly completes the space.                     //
  /// Aborts with boosted-dev; results in 3 branches in kore-rpc-booster.       //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test4-init]: <k> test4Init() => test4State1() ... </k>
                     <int> _ => ?_X                       </int>

  rule [test4-1-2]: <k> test4State1() => test4State2() ... </k>
                    <int> X                                </int>
                    requires X >Int 0
                    [priority(49)]

  rule [test4-1-3]: <k> test4State1() => test4State3() ... </k>
                    <int> X                                </int>
                    requires X <Int 0
                    [priority(49)]

  rule [test4-1-4]: <k> test4State1() => test4State4() ... </k>
                    <int> X        </int>
                    requires X ==Int 0


  ////////////////////////////////////////////////////////////////////////////////
  /// two hight-priorty rules apply with SAT remainder predicate,               //
  /// have to consider the remainder branch where X ==Int 0,                    //
  /// one rule at a lower priority applies unconditionally, which means that    //
  /// that the remainder is False. Rule test5-1-5 is unreachable.               //
  /// Aborts with boosted-dev; results in 3 branches in kore-rpc-booster.       //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test5-init]: <k> test5Init() => test5State1() ... </k>
                     <int> _ => ?_X                       </int>

  rule [test5-1-2]: <k> test5State1() => test5State2() ... </k>
                    <int> X        </int>
                    requires X >Int 0 [priority(48)]

  rule [test5-1-3]: <k> test5State1() => test5State3() ... </k>
                    <int> X        </int>
                    requires X <Int 0 [priority(48)]

  rule [test5-1-4]: <k> test5State1() => test5State4() ... </k>
                    [priority(49)]

  rule [test5-1-5]: <k> test5State1() => test5State5() ... </k>
                    <int> X        </int>
                    requires X ==Int 0

  ////////////////////////////////////////////////////////////////////////////////
  /// two hight-priorty rules apply with SAT remainder predicate,               //
  /// have to consider the remainder branch where X ==Int 0,                    //
  /// two rule at a lower priority applies unconditionally.                     //
  /// Aborts with boosted-dev; results in 4 branches in kore-rpc-booster.       //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test6-init]: <k> test6Init() => test6State1() ... </k>
                     <int> _ => ?_X                       </int>

  rule [test6-1-2]: <k> test6State1() => test6State2() ... </k>
                    <int> X        </int>
                    requires X >Int 0 [priority(48)]

  rule [test6-1-3]: <k> test6State1() => test6State3() ... </k>
                    <int> X        </int>
                    requires X <Int 0 [priority(48)]

  rule [test6-1-4]: <k> test6State1() => test6State4() ... </k>
                    [priority(49)]

  rule [test6-1-5]: <k> test6State1() => test6State5() ... </k>
                    [priority(49)]

// to produce input state:
// krun --output kore --depth 1 -cPGM='test1Init()' | kore-parser test-kompiled/definition.kore --module TEST --pattern /dev/stdin --print-pattern-json > state-test1Init.json
// then edit state-test1Init.json to substitute test1State1() for test1Init()

endmodule
