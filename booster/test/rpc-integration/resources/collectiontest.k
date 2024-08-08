module COLLECTIONTEST
  imports INT
  imports BOOL
  imports SET
  imports LIST
  imports MAP

  syntax State ::= State(Collection, Int) [symbol(State)]

  syntax Collection ::= Set // | List | Map

  configuration <k> $PGM:State ~> .K </k>

  rule [countdown-set]:
    <k> State(CURRENT:Set, N) => State(CURRENT SetItem(N) , N -Int 1) </k>
      requires N >Int 0 andBool notBool (N in CURRENT)
      [preserves-definedness]

endmodule
