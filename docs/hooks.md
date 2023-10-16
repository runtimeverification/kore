# Hooks

The builtin domains are listed below, categorized by sort.
Each sort and symbol is described and an example hooked declaration is given.
Note that some domains depend on others being defined!

## BOOL

No dependencies.

### BOOL.Bool

The builtin Boolean sort:

~~~
    hooked-sort Bool{}
        [hook{}("BOOL.Bool")]
~~~

Two domain values are recognized,

~~~
    \dv{Bool{}}("true")  // Boolean true
    \dv{Bool{}}("false")  // Boolean false
~~~

### BOOL.or

Logical OR: `\dv{Bool{}}("true")` if either operand is `\dv{Bool{}}("true")`, or
`\dv{Bool{}}("false")` otherwise.

~~~
    hooked-symbol or{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.or")]
~~~

### BOOL.and

Logical AND: `\dv{Bool{}}("true")` if both operands are `\dv{Bool{}}("true")`,
or `\dv{Bool{}}("false")` otherwise.

~~~
    hooked-symbol and{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.and")]
~~~

### BOOL.xor

Logical XOR: `\dv{Bool{}}("true")` if exactly one operand is
`\dv{Bool{}}("true")`, or `\dv{Bool{}}("false")` otherwise.

~~~
    hooked-symbol xor{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.xor")]
~~~

### BOOL.eq

`\dv{Bool{}}("true")` if the operands are equal, or `\dv{Bool{}}("false")`
otherwise.

~~~
    hooked-symbol eq{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.eq")]
~~~

### BOOL.ne

`\dv{Bool{}}("true")` if the operands are *not* equal, or `\dv{Bool{}}("false")`
otherwise.

~~~
    hooked-symbol ne{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.ne")]
~~~

### BOOL.not

Logical negation: `\dv{Bool{}}("true")` when its argument is
`\dv{Bool{}}("false")` and vice versa.

~~~
    hooked-symbol not{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.not")
~~~

### BOOL.implies

Logical implication.

~~~
    hooked-symbol implies{}(Bool{}, Bool{}) : Bool{}
        [hook{}("BOOL.implies")]
~~~

## INT

Depends on `BOOL`.

### INT.Int

The builtin sort of arbitrary-precision integers.

~~~
    hooked-sort Int{}
        [hook{}("INT.Int")]
~~~

Valid domain values are a string of decimal digits, optionally preceeded by a
sign.

~~~
    \dv{Int{}}("256")  // positive 256, sign omitted
    \dv{Int{}}("-1024")  // negative 1024
    \dv{Int{}}("+3")  // positive 3
~~~

### INT.gt

Comparison: is the first argument greater than the second?

~~~
    hooked-symbol gt{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.gt")]
~~~

### INT.ge

Comparison: is the first argument greater than or equal to the second?

~~~
    hooked-symbol ge{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.ge")]
~~~

### INT.eq

Comparison: is the first argument equal to the second?

~~~
    hooked-symbol eq{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.eq")]
~~~

### INT.le

Comparison: is the first argument less than or equal to the second?

~~~
    hooked-symbol le{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.le")]
~~~

### INT.lt

Comparison: is the first argument less than the second?

~~~
    hooked-symbol lt{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.lt")]
~~~

### INT.ne

Comparison: is the first argument inequal to the second?

~~~
    hooked-symbol ne{}(Int{}, Int{}) : Bool{}
        [hook{}("INT.ne")]
~~~

### INT.add

The sum of the arguments.

~~~
    hooked-symbol add{}(Int{}, Int{}) : Int{}
        [hook{}("INT.add")]
~~~

### INT.sub

The difference of the arguments (the first less the second).

~~~
    hooked-symbol sub{}(Int{}, Int{}) : Int{}
        [hook{}("INT.sub")]
~~~

### INT.mul

The product of the arguments.

~~~
    hooked-symbol mul{}(Int{}, Int{}) : Int{}
        [hook{}("INT.mul")]
~~~

### INT.abs

The absolute value of the argument.

~~~
    hooked-symbol abs{}(Int{}) : Int{}
        [hook{}("INT.abs")]
~~~

### INT.tdiv

Quotient of the first argument divided by the second (truncated toward zero).
The result is `bottom{}()` if the second argument is zero.

~~~
    hooked-symbol tdiv{}(Int{}, Int{}) : Int{}
        [hook{}("INT.tdiv")]
~~~

### INT.tmod

Remainder of the first argument divided by the second (truncated toward zero).
The result is `bottom{}()` if the second argument is zero.

~~~
    hooked-symbol tmod{}(Int{}, Int{}) : Int{}
        [hook{}("INT.tmod")]
~~~

### INT.ediv

Quotient of the first argument divided by the second (using the euclidean
algorithm).
The result is `bottom{}()` if the second argument is zero.

~~~
    hooked-symbol ediv{}(Int{}, Int{}) : Int{}
        [hook{}("INT.emod")]
~~~

### INT.emod

Remainder of the first argument divided by the second (using the euclidean
algorithm). The result is guaranteed to be positive.
The result is `bottom{}()` if the second argument is zero.

~~~
    hooked-symbol emod{}(Int{}, Int{}) : Int{}
        [hook{}("INT.emod")]
~~~

### INT.and

Bitwise and of the arguments.

~~~
    hooked-symbol and{}(Int{}, Int{}) : Int{}
        [hook{}("INT.and")]
~~~

### INT.or

Bitwise or of the arguments.

~~~
    hooked-symbol or{}(Int{}, Int{}) : Int{}
        [hook{}("INT.or")]
~~~

### INT.xor

Bitwise exclusive or of the arguments.

~~~
    hooked-symbol xor{}(Int{}, Int{}) : Int{}
        [hook{}("INT.xor")]
~~~

### INT.not

Bitwise complement of the argument.

~~~
    hooked-symbol not{}(Int{}) : Int{}
        [hook{}("INT.not")]
~~~

### INT.shl

Shift the bits of the first argument to the left with zeros shifted into
the lower bits. The second argument specifies how many bits to shift by, and
will be truncated to the least-significant Haskell Int. The second argument
can be negative, in which case the first argument will be shifted right. Right
shifts perform sign extension on signed numbers; i.e. they fill the top bits
with 1 if the first argument is negative and with 0 otherwise.

~~~
    hooked-symbol shl{}(Int{}, Int{}) : Int{}
        [hook{}("INT.shl")]
~~~

### INT.shr

Shift the bits of the first argument to the right. Right shifts perform sign
extension on signed numbers; i.e. they fill the top bits with 1 if the first
argument is negative and with 0 otherwise. The second argument specifies how
many bits to shift by, and will be truncated to the least-significant Haskell
Int. The second argument can be negative, in which case the first argument
will be shifted left with zeros shifted into the lower bits.

~~~
    hooked-symbol shr{}(Int{}, Int{}) : Int{}
        [hook{}("INT.shr")]
~~~

### INT.pow

The first argument raised to the power of the second argument. The result is
`bottom{}()` if the second argument is negative.

~~~
    hooked-symbol pow{}(Int{}, Int{}) : Int{}
        [hook{}("INT.pow")]
~~~

### INT.powmod

The first argument raised to the power of the second argument, but performed
modulo the third argument. The result is `\bottom{}()` if either
- The second argument is zero, or
- The second argument is negative and the first and third arguments are not
  coprime

~~~
    hooked-symbol powmod{}(Int{}, Int{}, Int{}) : Int{}
        [hook{}("INT.powmod")]
~~~

### INT.log2

The base 2 logarithm of the argument. The result is `bottom{}()` if the second
argument is not positive.

~~~
    hooked-symbol log2{}(Int{}) : Int{}
        [hook{}("INT.log2")]
~~~

## STRING

Depends on `BOOL`.

### STRING.String

The builtin String sort:

~~~
    hooked-sort String{}
        [hook{}("STRING.String")]
~~~

Valid domain values are strings.

~~~
    \dv{String{}}("string")
~~~

### STRING.eq

Comparison: are the two strings equal?

~~~
    hooked-symbol eq{}(String{}, String{}) : Bool{}
        [hook{}("STRING.eq")]
~~~

### STRING.lt

Comparison: is the first argument less than the second?

~~~
    hooked-symbol lt{}(String{}, String{}) : Bool{}
        [hook{}("STRING.lt")]
~~~

### STRING.concat

Concatenate two strings.

~~~
    hooked-symbol concat{}(String{}, String{}) : String{}
        [hook{}("STRING.concat")]
~~~

### STRING.string2int

Convert a base10 string to its integer value.

~~~
    hooked-symbol string2int{}(String{}) : Int{}
        [hook{}("STRING.string2int")]
~~~

### STRING.int2string

Convert an integer value to its base10 string representation.

~~~
    hooked-symbol int2string{}(Int{}) : String{}
        [hook{}("STRING.int2string")]
~~~

### STRING.string2base

Takes a string and a base and converts the string from `base` to its integer
value.
Supports bases 2 to 36 inclusive.

~~~
    hooked-symbol string2base{}(String{}, Int{}) : Int{}
        [hook{}("STRING.string2base")]
~~~

### STRING.base2string

Convert an integer value to its base string representation.

~~~
    hooked-symbol base2string{}(/* value */ Int{}, /* base */ Int{}) : String{}
        [hook{}("STRING.base2string")]
~~~

### STRING.substr

Substr takes a string and two indices (start and end) and returns the substring
that starts at index `start` and ends at index `end`.

~~~
    hooked-symbol substr{}(String{}, Int{}, Int{}) : String{}
        [hook{}("STRING.substr")]
~~~

### STRING.length

Calculates the length of a string.

~~~
    hooked-symbol length{}(String{}): Int{}
        [hook{}("STRING.length")]
~~~

### STRING.find

Takes a string, a substring and an index and searches for `substring` in
the `string`, starting at `index`, returning the resulting index, or `-1` if it
is not found.

~~~
    hooked-symbol find{}(String{}, String{}, Int{}): Int{}
        [hook{}("STRING.find")]
~~~

### STRING.token2string

Takes a value of any sort which is declared with the `hasDomainValues` attribute and returns that value unparsed.
For example: `\dv{TokenSort{}}("TOKEN")` is `"TOKEN"`, where `TokenSort{}` is a sort declared with the `hasDomainValues`
attribute.

~~~
    hooked-symbol token2string{}(TokenSort{}) : String{}
        [hook{}("STRING.token2string")]
~~~

### STRING.string2token

Takes a string and parses it into a token value of a hooked sort declared with the `hasDomainValues` attribute.
For example: `"TOKEN"` is `\dv{TokenSort{}("TOKEN")}`, where `TokenSort{}` is a sort declared with the `hasDomainValues`
attribute.

~~~
    hooked-symbol string2token(String{}) : TokenSort{}
        [hook{}("STRING.string2token")]
~~~

## MAP

Depends on `BOOL`.

The sorts of keys and values are arbitrary, but must be consistent between symbol
declarations. The sort consistency of hooked symbols is *not* checked. The key
and value sorts are referred to as `Key{}` and `Value{}` respectively below.

### MAP.Map

The builtin sort of key-value maps.

~~~
    hooked-sort Map{}
        [hook{}("MAP.Map")]
~~~

### MAP.unit

The empty map (containing no keys).

~~~
    hooked-symbol unit{}() : Map{}
        [hook{}("MAP.unit")]
~~~

### MAP.element

A map with one association, `key |-> value`.

~~~
    hooked-symbol element{}(Key{}, Value{}) : Map{}
        [hook{}("MAP.element")]
~~~

### MAP.concat

Combine two maps so that an association `key |-> value` is in the result if it
is in either operand. The result is `\bottom{}()` if the maps have any keys in
common.

~~~
    hooked-symbol concat{}(Map{}, Map{}) : Map{}
        [hook{}("MAP.concat")]
~~~

### MAP.update

Insert the association `key |-> value` into the map. If the key is already
present, replace the associated value with the new value provided.

~~~
    hooked-symbol update{}(Map{}, Key{}, Value{}) : Map{}
        [hook{}("MAP.update")]
~~~

### MAP.remove

Remove the key and the value associated with it from the map. If the key
is not present, the resulting map is the original map.

~~~
    hooked-symbol remove{}(Map{}, Key{}) : Map{}
        [hook{}("MAP.remove")]
~~~

### MAP.removeAll

Remove the `key |-> value` associations for which `key` is an element of
the set operand.

~~~
    hooked-symbol removeAll{}(Map{}, Set{}) : Map{}
        [hook{}("MAP.removeAll")]
~~~

### MAP.size

Get the size of the map

~~~
    hooked-symbol size{}(Map{}) : Int{}
        [hook{}("MAP.size")]
~~~

### MAP.lookup

If the given key is in the map, the result is the associated value; otherwise,
the result is `\bottom{}()`.

~~~
    hooked-symbol lookup{}(Map{}, Key{}) : Value{}
        [hook{}("MAP.lookup")]
~~~

### MAP.lookupOrDefault

If the given key is in the map, the result is the associated value; otherwise,
the result is the given default.

~~~
    hooked-symbol lookupOrDefault{}(Map{}, Key{}, /* default: */ Value{}) : Value{}
        [hook{}("MAP.lookupOrDefault")]
~~~

### MAP.in_keys

If the given key is in the map, the result is `\dv{Bool{}}("true")`; otherwise
the result is `\dv{Bool{}}("false")`.
~~~
    hooked-symbol inKeys{}(Key{}, Map{}) : Bool{}
        [hook{}("MAP.in_keys")]
~~~

### MAP.keys

Takes a map and returns a set of its keys.

~~~
    hooked-symbol keys{}(Map{}) : Set{}
        [hook{}("MAP.keys")]
~~~

### MAP.keys_list

Takes a map and returns a list of its keys.

~~~
    hooked-symbol keys_list{}(Map{}) : List{}
        [hook{}("MAP.keys_list")]
~~~

### MAP.values

Takes a map and returns a list of its values.

~~~
    hooked-symbol values{}(Map{}) : List{}
        [hook{}("MAP.values")]
~~~

### MAP.inclusion

If the first map is a submap of the second, the result is`\dv{Bool{}}("true")`;
otherwise the result is `\dv{Bool{}}("false")`.

~~~
    hooked-symbol inclusion{}(Map{}, Map{}) : Bool{}
        [hook{}("MAP.inclusion")]
~~~

## LIST

Depends on `INT`.

The sort of list elements is arbitrary, but must be consistent between symbol
declarations. The sort consistency of hooked symbols is *not* checked. The
element sort is referred to as `Elem{}` below.

### LIST.List

The builtin sort of associative lists.

~~~
    hooked-sort List{}
        [hook{}("LIST.List")]
~~~

### LIST.unit

The empty list.

~~~
    hooked-symbol unit{}() : List{}
        [hook{}("LIST.unit")]
~~~

### LIST.element

The singleton list.

~~~
    hooked-symbol element{}(Elem{}) : List{}
        [hook{}("LIST.element")]
~~~

### LIST.concat

Concatenate both arguments.

~~~
    hooked-symbol concat{}(List{}, List{}) : List{}
        [hook{}("LIST.concat")]
~~~

### LIST.get

Get an element from the list by index. Positive indices count from the beginning
of the list and negative indices count from the end. The first element is
`\dv{Int{}}("0")` and the last element is `\dv{Int{}}("-1")`. The result is
`\bottom{}()` if the index is out-of-bounds.

~~~
    hooked-symbol get{}(List{}, Int{}) : Elem{}
        [hook{}("LIST.concat")]
~~~

### LIST.update

Update an element from the list by index. The first element is `\dv{Int{}}("0")`.
The result is `\bottom{}()` if the index is out-of-bounds.

~~~
    hooked-symbol update{}(List{}, Int{}, Elem{}) : List{}
        [hook{}("LIST.update")]
~~~

### LIST.in

Is a value an element of the given list?

~~~
    hooked-symbol in{}(Elem{}, List{}) : Bool{}
        [hook{}("LIST.in")]
~~~

### LIST.size

The number of elements in the given list.

~~~
    hooked-symbol size{}(List{}) : Int{}
        [hook{}("LIST.size")]
~~~

### LIST.make

`LIST.make(n, x)` is a list of length `n` with `x` the value of every element.
The result is `bottom` if n is negative.

~~~
    hooked-symbol make{}(Int{}, Elem{}) : List{}
        [hook{}("LIST.make")]
~~~

## LIST.updateAll

`LIST.updateAll(L1, index, L2)` creates a new list using a list `L2` of size `N`
where the `N` elements of `L1` starting at `index` are replaced with the
contents of `L2`.
The result is `\bottom` in these cases:

- `index` is negative
- `L2` is non-empty and `index + N` exeeds the length of `L1`

~~~
    hooked-symbol updateAll{}(List{}, Int{}, List{}) : List{}
        [hook{}("LIST.updateAll")]
~~~

## SET

Depends on `BOOL`.

The sort of set elements is arbitrary, but must be consistent between symbol
declarations. The sort consistency of hooked symbols is *not* checked. The
element sort is referred to as `Elem{}` below.

### SET.Set

The builtin sort of sets.

~~~
    hooked-sort Set{}
        [hook{}("SET.Set")]
~~~

### SET.unit

The empty set.

~~~
    hooked-symbol unit{}() : Set{}
        [hook{}("SET.unit")]
~~~

### SET.element

The singleton set.

~~~
    hooked-symbol element{}(Elem{}) : Set{}
        [hook{}("SET.element")]
~~~

### SET.concat

The union of both arguments.

~~~
    hooked-symbol concat{}(Set{}, Set{}) : Set{}
        [hook{}("SET.concat")]
~~~

### SET.intersection

The intersection of both arguments

~~~
    hooked-symbol intersect{}(Set{}, Set{}) : Set{}
        [hook{}("SET.intersection")]
~~~

### SET.in

Is the element a member of the given set?

~~~
    hooked-symbol in{}(Elem{}, Set{}) : Bool{}
        [hook{}("SET.in")]
~~~

### SET.inclusion

Is the first set a subset of the second?

~~~
    hooked-symbol inclusion{}(Set{}, Set{}) :: Bool{}
        [hook{}("SET.inclusion")]
~~~

### SET.list2set

Convert a list to set

~~~
    hooked-symbol list2set{}(List{}) : Set{}
        [hook{}("SET.list2set")]
~~~

## KEQUAL

Depends on `BOOL`.

The sorts on which equality is tested are referred to as `Item`.

### KEQUAL.eq

Comparison: is the first argument equal to the second?

~~~
    hooked-symbol eq{}(Item{}, Item{}) : Bool{}
        [hook{}("KEQUAL.eq")]
~~~


### KEQUAL.neq

Comparison: is the first argument inequal to the second?

~~~
    hooked-symbol neq{}(Item{}, Item{}) : Bool{}
        [hook{}("KEQUAL.neq")]
~~~

### KEQUAL.ite

If-then-else: if condition then something, else something else.

~~~
    hooked-symbol ite{}(Bool{}, Item{}, Item{}) : Item{}
        [hook{}("KEQUAL.ite")]
~~~

## KRYPTO

All hash functions in this module interpret their input byte string as a raw sequence of 
8-bit bytes and output the digest in base-16 encoding
(a sequence of hexademical numerals `[0-9a-f]` each corresponding to 4 bits).

### KRYPTO.keccak256

Compute the Keccak-256 hash of the input string.

~~~
    hooked-symbol keccak256{}(Bytes{}) : String{}
        [hook{}("KRYPTO.keccak256")]
~~~

### KRYPTO.ripemd160

Compute the RIPEMD-160 hash of the input string.

~~~
    hooked-symbol ripemd160{}(Bytes{}) : String{}
        [hook{}("KRYPTO.ripemd160")]
~~~

### KRYPTO.sha256

Compute the SHA256 hash of the input string.

~~~
    hooked-symbol sha256{}(Bytes{}) : String{}
        [hook{}("KRYPTO.sha256")]
~~~

### KRYPTO.sha512_256raw

Compute the SHA512/256 hash of the input string, returning raw bytes.

~~~
    hooked-symbol sha512_256raw{}(Bytes{}) : Bytes{}
        [hook{}("KRYPTO.sha512_256raw")]
~~~

### KRYPTO.sha3256

Compute the SHA3-256 hash of the input string.

~~~
    hooked-symbol sha3256{}(Bytes{}) : String{}
        [hook{}("KRYPTO.sha3256")]
~~~

### KRYPTO.ecdsaPubKey

`ecdsaPubKey` takes a 32-character byte string of a private key, and returns the 64 byte hex-encoded public key.

~~~
    hooked-symbol ecdsaPubKey{}(Bytes{}) : String{}
        [hook{}("KRYPTO.ecdsaPubKey")]
~~~

## BYTES

Represents a sequence of bytes with update/get/padding operations as well as
conversions to/from strings.

### BYTES.bytes2string
Conversion to `String`.

~~~
    hooked-symbol bytes2string{}(Bytes{}) : String{}
        [hook{}("BYTES.bytes2string")]
~~~

### BYTES.string2bytes
Conversion to `Bytes`. Expects all characters to be in the '\0' .. '\255'
range.

~~~
    hooked-symbol string2bytes{}(String{}) : Bytes{}
        [hook{}("BYTES.string2bytes")]
~~~

### BYTES.update
Updates the byte array at the provided index with the provided value. Value
should be in the '\0' .. '\255' range.

~~~
    hooked-symbol update{}(Bytes{}, Int{}, Int{}) : Bytes{}
        [hook{}("BYTES.update")]
~~~

### BYTES.substr
Get a substring using start and end indices.

~~~
    hooked-symbol substr{}(Bytes{}, Int{}, Int{}) : Bytes{}
        [hook{}("BYTES.substr")]
~~~

### BYTES.replaceAt
Replace the contents within a byte array at the given index, with the provided
byte array.

~~~
    hooked-symbol replaceAt{}(Bytes{}, Int{}, Bytes{}) : Bytes{}
        [hook{}("BYTES.replaceAt")]
~~~

### BYTES.padRight
Pads the bytes string at the right to have at least of the specified length,
using the provided value.

~~~
    hooked-symbol padRight{}(Bytes{}, Int{}, Int{}) : Bytes{}
        [hook{}("BYTES.padRight")]
~~~

### BYTES.padLeft
Pads the bytes string at the left to have at least of the specified length,
using the provided value.

~~~
    hooked-symbol padLeft{}(Bytes{}, Int{}, Int{}) : Bytes{}
        [hook{}("BYTES.padLeft")]
~~~

### BYTES.reverse

~~~
    hooked-symbol reverse{}(Bytes{}) : Bytes{}
        [hook{}("BYTES.reverse")]
~~~

### BYTES.length

~~~
    hooked-symbol length{}(Bytes{}) : Int{}
        [hook{}("BYTES.length")]
~~~

### BYTES.concat

~~~
    hooked-symbol concat{}(Bytes{}, Bytes{}) : Bytes{}
        [hook{}("BYTES.concat")]
~~~

### BYTES.int2bytes

`BYTES.int2bytes` represents the _value_ as _length_ bytes in `BigEndian` or
`LittleEndian` order.

~~~
    hooked-symbol int2bytes{}(/* length */ Int{}, /* value */ Int{}, Endianness{}) : Bytes{}
        [hook{}("BYTES.int2bytes")]
~~~

### BYTES.bytes2int

`BYTES.bytes2int` decodes a `Signed` or `Unsigned` `Int` encoded as a
`BigEndian` or `LittleEndian` `Bytes`.

~~~
    hooked-symbol bytes2int{}(Bytes{}, Endianness{}, Signedness{}) : Int{}
        [hook{}("BYTES.bytes2int")]
~~~

### BYTES.decodeBytes

`BYTES.decodeBytes` supports `"UTF-8"`, `"UTF-16LE"`, `"UTF-16BE"`, `"UTF-32LE"`, and `"UTF-32BE"` as the first argument. If decoding and the input contains any invalid data then `bottom` is returned.

~~~
    hooked-symbol decodeBytes{}(/* decoding */ String{}, /* contents */ Bytes{}) : String{}
        [hook{}("BYTES.decodeBytes")]
~~~

### BYTES.encodeBytes

`BYTES.encodeBytes` supports `"UTF-8"`, `"UTF-16LE"`, `"UTF-16BE"`, `"UTF-32LE"`, and `"UTF-32BE"` as the first argument.

~~~
    hooked-symbol encodeBytes{}(/* encoding */ String{}, /* contents */ String{}) : Bytes{}
        [hook{}("BYTES.encodeBytes")]
~~~

## IO

Depends on `STRING`.

### IO.logString

Log string to terminal: The string argument is logged as a InfoUserLog log message. The hook returns `dotk{}()`.

~~~
    hooked-symbol logString{}(String{}) : K{}
        [hook{}("IO.logString")]
~~~
