# Purus Implementation plan

This document serves as an evidence of Milestone #1 completion for the original Catalyst proposal, and as an implementation plan for our subsequent work.

It contains a list of considerations / decisions on certain topics that we need to consider before we can proceed to the implementation.

## Data encoding

Data encoding refers to the way algebraic data types from PureScript are compiled down to UPLC code. Data definitions do not exist at runtime, but data constructors and destructors (pattern matching primitives) do.

### Row Type Polymorphism

Row type polymorphism is a language feature of PureScript that allows to pass extra type-level arguments that represent extra fields of a row or a record (records are implemented using rows in PS).

Syntactically, it looks like this:

```purescript
type FooRow (a :: Row Type) = (b :: c | a) -- `a` is the row type parameter

type FooRecord (a :: Row Type) = Record (FooRow a)

type FooRecord' (a :: Row Type) = { b :: c | a } -- equivalent to the above
```

Early on we realised that row type polymorphism makes the process of compiling significantly more complex, because with presence of a polymorphic record types we can't rely on any particular data layout when compiling.

For example, suppose we want to compile records down to tuples by sorting fields in a lexicographic order:

```
{ a: 1, b: "foo", c: unit } -> [1, "foo", unit]
```

Then a field access would be determined just by the index (here it is 1 because "b" is on the second position in `sort(["a", "b", "c"])`):

```
{ a: 1, b: "foo", c: unit }.b -> [1, "foo", unit] !! 1
```

With a polymorphic function like this we would not be able to get the layout of a UPLC list naively:

```purescript
projA :: forall (row :: Row Type). { a :: Int | row } -> Int
projA { a } = a
```

We have an insight for solving this problem, inspired by Haskell's `HasField` machinery, but it's just an idea and not something we fully developed: we can transform it into an extra parameter (here using a self-explanatory invented here syntax):

```purescript
projA :: forall (row :: Row Type). \indexOfAinRow -> { a @ indexOfAinRow :: Int | row } -> Int
projA ix record = record !! ix
```

This kind of parameters can be resolved at compile time (similar to subsumption of type class dictionaries), and probably inlined in (almost?) every use site.

### Data encoding techniques

There are three data encoding approaches we considered:

1. Scott encoding

TBD <!-- description -->

Pros: TBD

Cons: TBD

2. Sum of Products representation

Sum-of-products (SOP), is a more recent approach to data encoding specified in [CIP-0085](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0085).

Pros: TBD

Cons: TBD

3. PlutusData encoding

PlutusData encoding means using built-in `Data` primitive type that can represent any algebraic data type without function-values. This is the approach of Aiken.

Pros: TBD

Cons: TBD

### Our decisions

Our priorities:

1. Functions as data is crucial, so PlutusData encoding is not possible.

## LambdaBuffers support

[lambda-buffers](https://github.com/mlabs-haskell/lambda-buffers) is a schema language that can be used to generate encoders/decoders for multiple formats for multiple languages.

We decided to add LambdaBuffers support via codegen: since Purus datatypes are algebraic datatypes similar to those of PureScript, existing PureScript implementation can be trivially modified to support Purus. We will not include any LambdaBuffers-specific logic into the compiler, because we want to make it PlutusData-encoding agnostic: the user should have control over PlutusData representations of their types.

## Type classes implementation

Type classes are resolved at compile time. Resolution process guarantees that type class constraints are either erased completely or turned into a value argument (a record containing type class member implementations for a given type class instance).

for example,

```purescript
foo :: forall a m. Applicative m => a -> m a
foo x = pure x
```

can be translated to something like:

```purescript
foo Applicative_m_dict x = Applicative_m_dict.pure x
```

The dictionary can be defined per-type then (pseudo-syntax):

```purescript
Applicative_Array = { pure: \x -> [ x ] }
```

Thus a basic type class implementation requires only records (or even just product types) from the target language.

## Linker

PureScript compiles code on per-module basis, because it has to maintain per-module FFI interfaces.

TBD: how to link modules together

## Possible optimizations

TBD
<!-- Possible optimisations we could apply (list of approaches with links) -->

## Software environment

TBD

<!-- A list of assumptions about the software environment the compiler will rely on -->
