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

We have considered two approaches to solving this problem: 

#### Solution 1: Desugar to Typeclass 

The first is inspired by Haskell's `HasField` machinery and the `HasType` constraint from `row-types`. The general idea is to desugar open rows to a set of typeclass constraints that can be solved by the compiler for all and only closed rows. One possible implementation of the typeclass might be: 

```purescript
class HasType :: Symbol -> Type -> Row Type -> Constraint 
class HasType l t r | l r -> t where 
   lookup :: Record r -> t   -- will require a type application of the Symbol to use, could also pass proxies 
```

Desugaring the previous example would give us: 

```purescript 
projA :: forall (row :: Row Type). HasType "a" Int row => Record row -> Int 
projA record = lookup @"a" record 
```

In cases where the `row` type argument is known at compile time, the constraint can be solved using builtin compiler-solved typeclass utilities in conjunction with type inference. However, not _every_ constraint can be solved at compile time. For instance, consider a library module that contains a function with a type such as `f :: forall (r :: Row Type). {a :: Int} -> (...)`. The function `f` may not instantiate `r` to a concrete row type until it is imported and applied to a concrete argument. This poses a difficulty for us, because, as far as we are aware, there is no way to represent the _kind_ of `r` using the PIR/TPLC type system (which, to our knowledge, supports higher kinded types but not lifted datatypes). One possible solution is to transform the previous example into something like: 

```purescript 
projA :: forall (t :: Type). (t -> Int) -> t -> Int 
projA f record = f record 
```

This yields a function with a type that can be represented in the Plutus type system. While this erases information (i.e. concerning labels) necessary to solve the constraint, we could conceivably encode this in annotation metadata when generating PIR or TPLC and resolve the "implicit" function argument during linking. 

#### Solution 2: Defer Compilation

The second solution is, essentially, to do nothing until we reach the point in our compilation pipeline where all type variables are (or should be) instantiated. To elaborate: The type of a Plutus script (a validator or minting policy) never contains type variables and lacks quantifiers. Consequently, any row-polymorphic expression must be instantiated with a concrete row if it is used to write a Plutus script. 

Because the most severe problem with the typeclass approach is that it requires us to generate PIR or TPLC that contains types (kinds, to be more specific) that cannot be represented (without losing information) in the PIR/TPLC type system, one way to solve this problem _without_ adding subsantial complexity during the linking phase is simply to defer conversion into PIR/TPLC until we know we are compiling a Plutus script, at which point all types must be fully instantiated. If all types are fully instantiated, then records (and functions that contain record updates and accessors) can easily be transformed into PIR products (in accordance with the rough procedure outlined above). 

One potential drawback of defering compilation is that doing so prohibits us from generating PIR or TPLC _modules_ directly. On this approach, our first compilation pass would generate modules in a type-annotated variant of PureScript's `CoreFn` AST (possibly with additional support for type declarations). These modules could be bundled as libraries and used by other developers, but could not be used to directly generate _arbitrary_ PIR/UPLC. A second compilation pass - which aims at generating a UPLC Plutus script - would resolve imports, perform inlining and some optimizations, instantiate all type variables (& other misc linking tasks) _at the `CoreFn`_ level, before transforming typed `CoreFn` to `PIR` and, finally, UPLC.  

#### Decision 

The only drawback to Solution 2 is that it forces us into a particular design for modules and the linker. However, we see no advantage in generating PIR/TPLC modules directly and no disadvantage in generating (typed) `CoreFn` modules. The added complexity of the typeclass approach simply does not provide advantages proportionate with the amount of labor required to implement it, and therefore we will be moving forward with Solution 2. 


### Data encoding techniques

There are three data encoding approaches we considered:

1. Scott encoding

Scott encoding is a technique (or family of techniques) for representing algebraic data types as lambda calculus terms (i.e. functions). Although there is no canonical form or standard set of techniques for working with Scott-encoded data, the general idea behind the encoding is to store values (e.g. arguments to a constructor) as parameters to a function. The parameters can then be accessed by applying a suitable "destructor" function to the lambda term which stores the values. 

Scott encoding is used in Plutarch and (as far as I am aware - this may change) PlutusTx. While a full explanation of Scott encoding is outside the scope of this report, the [Plutarch Docs](https://plutonomicon.github.io/plutarch-plutus/Concepts/Data%20and%20Scott%20encoding) provides a simple example that showcases the approach (comments added here, minor changes for clarity): 

``` haskell
-- A Scott-encoded variant of Haskell's Maybe type 
type Maybe a = forall b. (a -> b) -> b -> b

-- Constructs a Just value of type Maybe 
just :: a -> Maybe a
just x = \f _b -> f x -- Note the types (ignoring the quantifier, pretending we have an arbitrary `b` in scope): f :: (a -> b), _b :: b

nothing :: Maybe a
nothing = \_f n -> n -- _f :: (a -> b), n :: b 
```

Pros:
  - Scott encoding is very performant. Either it is the most performant encoding considered here, or it is at worst a few percent less performant than the most performant encoding 
  - Because data structures are just lambda terms, Scott encoding enables storing functions in data structures 
  - Reliable: Used in a wide variety of live smart contracts written in PlutusTx or Plutarch 

Cons:
  - Not standardized: As mentioned above, Scott encoding is a family of encodings or set of techniques. There may be multiple acceptable Scott-encoded equivalents to an arbitrary type from an arbitray source language, but these equivalents are not necessarily identical to or interchangable with one another. 
  - Not intuitive: Scott-encoded terms are difficult for humans, even experienced functional programmers, to parse and reason about. For nontrivial data types, it is both extremely difficult to infer the corresponding type from PLC code _and_ impossible to determine the Scott-encoded equivalent by looking at (e.g.) the Haskell data type that it corresponds to - as terms may have more than one Scott-encoded equivalent, one has to examine the machinery that performs the conversion in order to discover the Scott representation
  - _Possibly_ inferior in performance to the SOP representation (though this is difficult to determine definitively without better benchmarks than those available at present)
  - Somewhat difficult to lift expressions from a source language, virtually impossible to unlift Scott-encoded expressions back to a target language
  - Not a "native format": The arguments which are applied to a validator during script execution are presented in the data-encoded format, and so must either be converted to the Scott-encoded representation or worked with directly, using specialized functions for data-encoded values. It is often difficult to determine in advance which option results in the most performant/efficient scripts.

2. Sum of Products representation

Sum-of-products (SOP), is a more recent approach to data encoding specified in [CIP-0085](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0085). Generally speaking, the SOP encoding represents algebraic data types as a list-of-lists-of-types (`[[Type]]`), where the elements of the outer list represents constructors (i.e. of a sum type) and the elements of each inner list represent arguments to each constructor (i.e. products or tuples). The SOP encoding of an arbitrary type constitutes a normal form (which, incidentally, corresponds via the Curry-Howard isomorphism to disjunctive normal form in boolean logic), and therefore each data type has a canonical representation (assuming that constructors are ordered) in the SOP encoding. 

Pros:
  - _Possibly_ the most performant encoding, as indicated by benchmarks in the CIP 
  - Standardized: Every algebraic data type (assuming that constructors are ordered) has a single canonical representation in the SOP encoding 
  - Intuitive: The SOP representation of data types is used in several Haskell generic programming libraries, most prominently [generics-sop](https://hackage.haskell.org/package/generics-sop), and therefore many functional programmers are likely to be familiar with it. Because the SOP representation is a normal form, it is much easier to determine the SOP-encoded equivalent to a PureScript type, which makes it easier to reason about performance without examining the machinery that performs conversions. 
  - Much easier lifting and unlifting (as explained in the CIP)
  - Like Scott encoding, enables storing functions in data structures, which is essential given our PureScript frontend 
  - May see future performance increases, e.g. O(1) indexing of constr arguments (based on conversations with IOG)
  
Cons:
  - Not a "native" format (see discussion of this con in the Scott encoding section)

3. PlutusData encoding

PlutusData encoding means using built-in `Data` primitive type that can represent any algebraic data type without function-values. This is the approach of Aiken.

Pros:
  - Is the "native" format in which arguments to scripts are passed. Does not require conversion, which may lead to superior performance (especially for simple scripts)
  - Intuitive: Conceptually similar to JSON, with which every developer is (or ought to be) familiar 

Cons:
  - Poor performance 
  - Cannot store functions in data

### Our decisions

Our priorities:

1. Functions as data is crucial, so PlutusData encoding is not viable.
2. All other things being equal, a standardized & intuitive representation is superior to a non-standardized or non-intuitive representation. 

In light of our priorities, the SOP representation is the best choice for Purus. While Scott encoding is a viable option in that it is performant and enables storing functions in data structures, it lacks any distinct advantage over the SOP representation. 

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

PureScript compiles code on a per-module basis, because it has to maintain per-module FFI interfaces. 

Modifying the PureScript compiler to behave differently would be extremely laborious and error-prone - the assumption of multiple output modules is deeply embedded in the PureScript compilation pipeline. Making a change would require a near-total rewrite of that pipeline, which we do not judge to be viable given our budget and resources. 

Any linker design, then, must satisfy the following constraints: 
  - It must preserve the per-module compilation pipeline of the PureScript compiler 
  - It must yield modules that can serve as libraries
  - It must be capable of assembling multiple library modules into a single "executable" Plutus script 
  - It must be consistent with our solutions to the row polymorphism and data encoding problems 
  
One obvious solution satisfies all of these constraints: A two-stage compilation process that, in the first stage, outputs an intermediate representation on a per-module basis and, in the second stage, resolves imports, performs inlining (&etc) to generate a UPLC executable. This two-stage design satisfies all of the above criteria, and has the additional benefit of minimizing the required changes to the PureScript compiler (i.e. the second stage could be implemented as a separate executable in a different project). 

While we still have a few details to flesh out, the rough shape of our compilation pipeline architecture is: 
  1. The PureScript compiler parses the CST from a set of module files and transforms it into a (sugared) AST (this is unmodified PS compiler behavior)
  2. We modify the PureScript compiler to transform the sugared AST into a *typed* variant of PureScript's `CoreFn` AST. The simplest way to do this is to tag expressions with their type in the annotation field, though it may be useful to create our own variant AST for ergonomic reasons. 
  3. We write a trivial code generator that serializes typed `CoreFn` modules and writes them to the output directory. The compiler will also generate suitable `ExternsFile`s, which are needed for incremental compilation and linking. (This is the last step that involves the PS compiler directly)
    - The set of `CoreFn` modules generated at this stage can serve as a package or library which may be shared  
  4. We implement a second-pass compiler executable which: 
    - Parses the `Main` source file (which should contain a `main` function that will be compiled to a UPLC script) and transforms it into typed `CoreFn`
    - Parses the `CoreFn` modules and `ExternsFile`s for modules imported by the `Main` source file 
    - Resolves imports 
    - Performs inlining 
      - We probably _have_ to inline row-polymorphic functions because we cannot represent their types in a PIR bindings/declaration
      - We probably do not need to inline monomorphic or `Type`-polymorphic functions, for which we can generate PIR bindings/declarations 
    - Applies all type arguments / instantiated all type variables (throwing an error if a variable cannot be instantiated to a concrete type)
    - Eliminates `ObjectUpdate` and `Accessor` expressions using fully-instantiated (closed) row types 
    - Performs any optimizations
    - Transforms the typed `CoreFn` AST into a PIR AST 
    - Compiles PIR to UPLC
    - Seralizes the UPLC Plutus Script and writes it to the output directory 


## Possible optimizations

TBD
<!-- Possible optimisations we could apply (list of approaches with links) -->

## Software environment

TBD

<!-- A list of assumptions about the software environment the compiler will rely on -->
