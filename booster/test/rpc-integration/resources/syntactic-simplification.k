module SYNTACTIC-SIMPLIFICATION
  imports INT
  imports BOOL

  syntax State ::= test1Init()
                 | test1State1()
                 | test1State2()

  configuration <k> $PGM:State ~> .K </k>
                <int-x> 0:Int        </int-x>
                <int-y> 0:Int        </int-y>

  ////////////////////////////////////////////////////////////////////////////////
  //
  ////////////////////////////////////////////////////////////////////////////////
  rule [test1-init]: <k> test1Init() => test1State1() ... </k>
                     <int-x> _         => ?X              </int-x>
                     <int-y> _         => ?Y              </int-y>
                     ensures  ?X <Int 100
                      andBool ?Y <Int 50
                      andBool ?X <Int ?Y

  rule [test1-1-2]: <k> test1State1() => test1State2() ... </k>
                    <int-x> X                              </int-x>
                    requires X <Int 50

  rule [test1-simplify]:
    X <Int Y => true
    requires X <Int Z
     andBool Z <Int Y
     // [simplification, syntactic(1)]
     [simplification]

// to produce input state:
// krun --output kore --depth 1 -cPGM='test1Init()' | kore-parser test-kompiled/definition.kore --module TEST --pattern /dev/stdin --print-pattern-json > state-test1Init.json
// then edit state-test1Init.json to substitute test1State1() for test1Init()

endmodule
