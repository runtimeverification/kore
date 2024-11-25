module SUBK
  imports BOOL
  imports K-EQUAL

  syntax State ::= "ping" [token]
                 | "pong" [token]
                 | "peng" [token]

  configuration <k> $PGM:State ~> .K </k>
                <x> .K </x>

  syntax State ::= f ( State ) [function]

  syntax K     ::= inK ( State) [function, total]

  rule f(ping) => ping
  rule f(pong) => pong
  // f is not total

  rule inK(STATE) => STATE ~> .K

  rule [ping]: <k> ping => f(pong) ... </k>
               <x> X </x>
    requires notBool (X ==K inK(ping))

  rule [pong]: <k> pong => f(ping) ... </k>
               <x> X </x>
    requires notBool (X ==K inK(pong))

endmodule
