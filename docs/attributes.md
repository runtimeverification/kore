# Attributes

As pointed out in
[Attributes as Patterns](<../design-decisions/2018-07-25 Attributes as patterns.md>),
attributes are encoded as Kore patterns that only use application and string
literals.

## Axiom Pattern Attributes

Kore.Attribute.Axiom

Attributes specific to interpreting axiom patterns.

### Assoc

Associativity axiom

```
assoc{}()
```

Example, List concatenation

```
axiom{R} \equals{SortList{}, R} (
  Lbl'Unds'List'Unds'{}(
    Lbl'Unds'List'Unds'{}(K1:SortList{},K2:SortList{}),
    K3:SortList{}),
  Lbl'Unds'List'Unds'{}(
    K1:SortList{},
    Lbl'Unds'List'Unds'{}(K2:SortList{},K3:SortList{}))) [assoc{}()]
```

### Comm

Commutativity axiom

```
comm{}()
```

Example: Map concatenation

```
axiom{R} \equals{SortMap{}, R} (
  Lbl'Unds'Map'Unds'{}(K1:SortMap{},K2:SortMap{}),
  Lbl'Unds'Map'Unds'{}(K2:SortMap{},K1:SortMap{})) [comm{}()]
```

### Concrete

```
concrete{}()
```

Concrete axioms are not applied to symbolic patterns (patterns containing
variables).


### HeatCool

Heating and cooling axiom

```
heat{}()
cool{}()
```

In each execution step, the execution strategy first heats the configuration,
applies the normal rules (without `heat{}()` and `cool{}()`), and cools the
result.


### Idem

Idempotency axiom

```
idem{}()
```

Example: concatenation of identical Sets

```
axiom{R} \equals{SortSet{}, R} (
  Lbl'Unds'Set'Unds'{}(K:SortSet{},K:SortSet{}),K:SortSet{}) [idem{}()]
```

### Owise

```
owise{}()
```

This rule will apply only if the term does not unify with any other rules.
Has default priority 200.

### ProductionID

```
productionID{}("id")
```

### Priority

```
priority{}("123")
```

The priority attribute specifies a number which determines the order of rule
application. The attribute's argument is a string literal representing a decimal
integer.

### Simplification

```
simplification{}()
```

There are 2 types of evaluation: definition evaluations that implement the
actual semantics and simplification evaluations which are not the part of the
semantics, but can be used for simplifying the configuration before applying
definitions. For example, `(x + y) + z = (x + z) + y` where `x` and `z` are
concrete and `y` is not concrete.


### Trusted

```
trusted{}()
```

Spec axioms with this attribute are excluded from verification.

### Unit

Unit element axiom

```
unit{}()
```

### Unique ID

Unique identifier for axioms, usually assigned by the frontend at compile time.

```
UNIQUE'Unds'ID{}("07a34b11585162c291311c03441e08beb2532e48d")
```


## Symbol Attributes

Kore.Attribute.Symbol.Symbol

Attributes assigned to symbols.

### Anywhere

```
anywhere{}()
```

A symbol is given the `anywhere` attribute when it has `anywhere` rules
associated with it.

### Constructor

```
constructor{}()
```

A symbol is injective if it is given the `injective` attribute, the
`constructor` attribute, or the `sortInjection` attribute.

### Function

```
function{}()
```

A symbol is a function if it is given the `function` attribute.

### Total

```
total{}()
```

A symbol is total-function-like if it is given the `total` + `function` attributes.

### Hook

```
hook{}("BUILTIN.name")
```

The string `"BUILTIN.name"` is used to lookup for [known builtins](./hooks.md).

### Injective

```
injective{}()
```

A symbol is injective if it is given the `injective` attribute, the
`constructor` attribute, or the `sortInjection` attribute.

### Memo

```
memo{}()
```

The `memo` attribute signals that the backend may cache, or memoize, the result
of a symbol application (usually a `function`).

### Smtlib

The `smtlib` attribute specifies how a Kore application pattern built with the
symbol can be translated to SMT-LIB 2 list expression for an external SMT
solver.

```
smtlib{}("⟨S-expression⟩")
```

#### ⟨S-expression⟩ is an atom

```
smtlib{}("max")
```

The atom is used as the head of the list and the arguments of the application
are passed as arguments for the list.

#### ⟨S-expression⟩ is a list

```
smtlib{}("(mod (^ #1 #2) #3)")
```

The list may contain special meta-variables `#1`, `#2`, ... (not a valid
SMT-LIB syntax) which indicate the position of the arguments of the application
pattern.

### Smthook

The `smt-hook` is similar to [`smtlib`](#Smtlib), except that it's meant to
specify that the symbol will be translated to a predefined SMT operator, so
the SMT operator needs not be declared.

### SortInjection

```
sortInjection{}()
```

A symbol is injective if it is given the `injective` attribute, the
`constructor` attribute, or the `sortInjection` attribute.

A symbol is total-function-like if it is given the `total` + `function` attributes or the
`sortInjection` attribute.

## misc.

### Location

```
org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(sl,sc,el,ec)")
```

Used for defining the location of the current declaration in the original file.
The numbers represent start line (sl), start column (sc), end line (el), and end
column (ec).

### Source

```
org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(FilePath)")

```

Used for defining the source (K) file of the current declaration. 'FilePath' is
the full path to the original file.

### Subsort

```
subsort{Sub,Super}()
```

Used to define subsorting relations, attached to subsort axiom. For example,

```
axiom{R} \exists{R} (Val:SortKResult{},
  \equals{SortKResult{}, R}
    (Val:SortKResult{},
     inj{SortBool{}, SortKResult{}} (From:SortBool{})))
  [subsort{SortBool{}, SortKResult{}}()]
```
