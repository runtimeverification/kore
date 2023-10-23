module ECDSA
    imports INT
    imports BYTES
    imports STRING
    imports BOOL

    syntax String ::= Keccak256 ( Bytes ) [function, hook(KRYPTO.keccak256)]
                    | ECDSAPubKey ( Bytes ) [function, hook(KRYPTO.ecdsaPubKey)]
 // -----------------------------------------------

    syntax Int ::= #addrFromPrivateKey ( String ) [function, klabel(addrFromPrivateKey)]
 // ------------------------------------------------------------------------------------
    rule [addrFromPrivateKey]: #addrFromPrivateKey ( KEY ) => #addr( #parseHexWord( Keccak256( #parseByteStack( ECDSAPubKey( #parseByteStack( KEY ) ) ) ) ) )

    syntax Int ::= #parseHexWord ( String ) [function]
 // --------------------------------------------------
    rule #parseHexWord("")   => 0
    rule #parseHexWord("0x") => 0
    rule #parseHexWord(S)    => String2Base(replaceAll(S, "0x", ""), 16) requires (S =/=String "") andBool (S =/=String "0x")

    syntax Bytes ::= #parseByteStack   ( String ) [function, memo]
                   | #parseHexBytes    ( String ) [function]
                   | #parseHexBytesAux ( String ) [function]
 // --------------------------------------------------------
    rule #parseByteStack(S) => #parseHexBytes(replaceAll(S, "0x", ""))

    rule #parseHexBytes(S)  => #parseHexBytesAux(#alignHexString(S))
    rule #parseHexBytesAux("") => .Bytes
    rule #parseHexBytesAux(S)  => Int2Bytes(lengthString(S) /Int 2, String2Base(S, 16), BE)
      requires lengthString(S) >=Int 2

    syntax String ::= #alignHexString ( String ) [function, total]
 // --------------------------------------------------------------
    rule #alignHexString(S) => S             requires         lengthString(S) modInt 2 ==Int 0
    rule #alignHexString(S) => "0" +String S requires notBool lengthString(S) modInt 2 ==Int 0

    syntax String ::= #unparseData      ( Int, Int  ) [function]
                    | #unparseDataBytes ( Bytes )     [function]
 // ------------------------------------------------------------
    rule #unparseData( DATA, LENGTH ) => #unparseDataBytes(#padToWidth(LENGTH,#asByteStack(DATA)))

    rule #unparseDataBytes( DATA ) => replaceFirst(Base2String(#asInteger(#asByteStack(1) +Bytes DATA), 16), "1", "0x")

    syntax Int ::= #asInteger ( Bytes ) [function, total]
 // -----------------------------------------------------
    rule #asInteger(WS) => Bytes2Int(WS, BE, Unsigned) [concrete]

    syntax Bytes ::= #asByteStack ( Int ) [function, total]
 // -------------------------------------------------------
    rule #asByteStack(W) => Int2Bytes(W, BE, Unsigned) [concrete]

    syntax Bytes ::= #padToWidth      ( Int , Bytes ) [function, total]
                   | #padRightToWidth ( Int , Bytes ) [function, total]
 // -------------------------------------------------------------------
    rule                            #padToWidth(N, BS)      =>               BS        requires notBool (N >=Int 0)
    rule [padToWidthNonEmpty]:      #padToWidth(N, BS)      =>  padLeftBytes(BS, N, 0) requires          N >=Int 0
    rule                            #padRightToWidth(N, BS) =>               BS        requires notBool (N >=Int 0)
    rule [padRightToWidthNonEmpty]: #padRightToWidth(N, BS) => padRightBytes(BS, N, 0) requires          N >=Int 0

    syntax Int ::= #addr ( Int ) [function]
 // ---------------------------------------
    rule #addr(W) => W %Word pow160

    syntax Int ::= Int "%Word" Int [function, total]
 // ------------------------------------------------
    rule  _ %Word W1 => 0            requires W1  ==Int 0
    rule W0 %Word W1 => W0 modInt W1 requires W1 =/=Int 0

    syntax Int ::= "pow160" [alias] /* 2 ^Int 160 */
 // ------------------------------------------------
    rule pow160 => 1461501637330902918203684832716283019655932542976

    configuration
       <k> $PGM:Pgm </k>

    syntax Pgm ::= Bytes | String | Int
 // -----------------------------------

endmodule
