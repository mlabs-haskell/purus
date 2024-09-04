# Architecture

This document contains an overview of the *current* architecture of our compiler pipeline. It is not meant to be exhaustive and is intended to serve as a guide for onboarding new developers / make the giant PRs more accessible. 


## Step 1: Type Deduction/Synthesis (Language.PureScript.CoreFn.Desugar)

During the research phase of this project, we determined that PIR (Plutus Intermediate Representation) should be our ultimate compilation target (with final compilation to UPLC handled by the PIR compiler). Because PIR is an explicitly typed language, and because the vanilla `CoreFn` AST is not explicitly typed, it is necessary to convert the `Language.PureScript.AST` AST into a typed variant. 

Because conversion to `CoreFn` occurs after the initial (vanilla PS) typechecker pass, we receive *most* expressions (but not all of them) annotated with the inferred or explicitly declared type. Desugared expressions or declarations involving type classes, however, are not ignored by the PS typechecker, but we require explicit annotations for explicit type class dictionaries and the functions that operate on them. 

This step consists of two main phases: 

  - First, we traverse every *type* annotation in the vanilla PS `Module` that is the input to our conversion function and desugar constraint types to types that require explicit dictionary arguments. E.g. `Eq a => a -> (...)` becomes `Eq$Dict a -> a -> (...)`. 
  - Next, we deduce the types. Because top-level declarations are always explicitly typed, we proceed "top-down" from the top-level signature as far as we can, and switch to a "bottom-up" synthesis strategy in cases where the type cannot be determined by the top-level signature. 

Some notes: 

  - We want to preserve quantifiers in this step. While some quantifiers can be eliminated during monomorphization, there is no guarantee that *all* of them will be eliminated (this is not a problem, because PIR supports universal quantification). This requires special machinery when performing type deduction on expressions that contain quantified polymorphic functions, since we must take care to ensure that the type variables "line up". 
  - We run our desugarer in the same monad stack as the PS TypeChecker, but this is largely a convenience (since it lets us use the PS machinery for binding local variables or type variables, etc). We should *never* perform type inference or make any calls to `unify`. 
  - The trickiest part of this is ensuring that type information for explicit type class dictionaries is introduced at the correct point. Again, type class dictionaries are not processed by the PS typechecker. 
  
## Step 2: Monomorphization & Inlining (Language.PureScript.CoreFn.Convert.Monomorphize)

PureScript's implementation of Records employs Row Types. Moreover, PureScript supports *polymorphic* records, which are backed by *open rows* (e.g. `{a :: Foo | r}`). 

Records in PureScript (unlike in Haskell) are not just syntatic sugar for products - the order of fields in the record is *not* determined by the order of fields in the declaration or expression that introduces the record. *An* order can be established for any fully instantiated (i.e. closed - doesn't contain any type variables of kind `Row Type`) record types - we choose a lexicographic ordering to mirror PureScript's `RowToList` sorting but this is not essenital. 

We must, therefore, perform monomorphization on record types in order to transform Object Literals to literal products, transform record accessors into `case` analysis of the product that corresponds to the original record, and transform record updates into product updates. 

Because a single polymorphic record type may be instantiated to different concrete types at different places in the AST, we must also inline while we monomorphize. 

The general monomorphization procedure is as follows (not a full algorithm): 

  1. We traverse the AST until we reach an `App` node. 
  2. We "peel" the App node and get an `(f,args)` where `f` is the "terminal" function expression and `args` is a list of the argument expressions to which it is applied. 
  3. We check whether the type of `f` is already monomorphic. If it is, we make no change to the node. 
  4. If `f` is not monomorphic, we strip the quantifiers and check whether we can specialize any bound type variables to a concrete type. E.g. if `f :: forall x. Tuple x x -> x -> Int` and the arguments are `[Tuple String String, String]`, we instantiate `x` to `String`. 
  5. We recurse down the list of arguments. If we encounter an argument that is a free (term-level) variable that is *not* a Plutus builtin, we inline it, monomorphizing it to the concrete type of the argument if possible.  
  6. We apply the (possibly monomorphized & inlined) argument to the function and procede to the next argument 
     until we run out of arguments to process.
  7. Finally, we re-quantify the type of `f` in the resulting expression. (It may contain free type variables of kind `Type`, which can be represented in PIR)
    
The procedure for inlining is: 
  1. We check whether the (term level) variable we aim to inline is locally scoped (i.e. qualified by source position) or globally scoped (e.g. qualified by module name). For now, we only attempt to inline globally scoped variables (...I forget why...)
  2. If the variable is globally scoped (i.e. is the name of a module-level declaration), we lookup the body of the declaration. There are two possibilities here: Either the declaration will be a single non-recursive binding, or it will be a member of a mutually recursive binding group. (PS compiler sorts these for us) 
    - If the declaration is non-recursive we "walk" its expression-body and monomorphize/inline the sub-expressions as necessary in order to properly assign the expression the type that it is to be monomorphized to. 
    - If the declaration the member of a recursive binding group, we pause inlining, walk the expression, and "collect" a `Map Name (Name,SourceType,Expr)` where the key is the original name of the expression, and the values are, respectively: The new "fresh" name to give to the monomorphized expression, the monomorphic type that we must assign the expression to, and the body of the declaration. We do this recursively until we have collected every member of the recursive binding group used in the target expression. Finally, we use that map to construct a *monomorphic* mutually recursive binding group (where the names are all fresh) and create a `Let`-binding for the monomorphized mutually recursive group. 

The implementation of the monomorphizer/inliner consists in a few key functions that call each other recursively. To make reviewing easier, here's a brief explanation of what the key functions do (or are supposed to do at any rate): 

  - `monomorphizeA` is the entry point that checks whether the node is an `App`, peels the arguments and function parts from the `App`, and calls `handleFunction` on the function expression and its arguments. 
  - `handleFunction` branches depending on the function expression it is passed: 
    - If it is passed a `Var` qualified by modulename, and the modulename is `Builtin`, it just returns the variable (since Builtins cannot be meaningfully inlined). 
    - If it is passed an `Abs`, `handleFunction` tries to instantiate the type variables of the function type with corresponding concrete types of the arguments. If it succeeds, it subsitutes the concrete type in for the bound type variable in all sub-expressions and their types, then calls itself recursively on the body of the `Abs` until the type has been fully monomorphized. 
    - If it is passed a `Var` that is not a builtin, `handleFunction` attempts to inline the expression the `Var` refers to by calling `inlineAs` with the monomorphized type. If this succeeds, `handleFunction` calls itself recursively with the monomorphized/inlined expression. 
    - If it is passed anything else as the `f`, it checks whether the function type is monomorphic. 
      - If the `f` is monomorphic, it applies it to its arguments and returns the resulting expresion. 
      - If the `f` is not monomorphic, `handleFunction` throws an error. 
  - `inlineAs` performs inlining and implements the above algorithm. Note that `inlineAs` is passed a PS `Type` argument, which represents the type that the expression corresponding to the `Name` being inlined *should* have after inlining. 
  - `monomorphizeWithType` rewrites the type of an expression to match the supplied type and (much more importantly) rewrites the types of all sub-expressions to conform with the supplied type of the top-level expression. 
  
## Step 3: Object desugaring and final IR (Language.PureScript.CoreFn.[IR / DesugarObjects])

By the end of step 2, all polymorphic records have been monommorphized that can be monomorphized, but record-specific expression nodes (object updates/accessors/literals) still remain in the AST. In order to ensure that all "invalid" expressions and types have been desugared/eliminated prior to final PIR conversion, we define a restricted AST and `Type` type such that only expressions which can be converted into PIR can be represented - a kind of "parse-don't-validate" approach. (This AST is implemented with the `Bound` library.) 

At this stage, we construct a set of dictionaries for type and data constructors. When constructing these maps, we add an arbitrary number of constructors for anonymous products (i.e. tuples) to accommodate objects. Specifically, we add constructors for tuples of up to 100 (they look like `data $GEN.~Tuple1 a = $GEN.~Tuple1 a` etc). These dictionaries serve two purposes: 
  - We construct them in such a way that the SOP representation of the underlying data type is very explicit. I.e. for each *type* constructor, we construct an `[(Int,[Type])]` where the `Int` represents the corresponding data constructor's index in the data type, where this information (the constructor's index & arguments) is also available in the dictionary for each *data* constructor. (Due to the implementation of the PS AST, we need both of these dictionaries, even though in principle only the tycon dictionary is necessary)
  - Embedding generated tuple types into these dictionaries allows us to treat desugared records as "normal" data types in the final compilation stage (i.e. they don't require any special handling). 
  
Conversion into this restricted IR AST is, aside from object expressions, very straightforward. Therefore, in the rest of this section I will explain the object desugaring process. 

### Object Literals 

  1. We extract a `Map PSString SourceType` from the object literal expression by inspecting the expressions in each field. 
  2. We sort the fields lexicographically (which establishes the uniform order of the product that will be constructed). 
  3. We generate "fake" names for the product's type and constructor. E.g. `$GEN.~Tuple2 a b`. 
  4. We construct a specialized function type for the product's data constructor using the sorted arguments. 
  5. We assemble the literal with the "fake" constructor function and the arguments. 
  
### Record Accessors 
  1. We perform steps 1-4 of Object Literals conversion on the expression the accessor is being applied to, except we construct a Constructor `Binder` (a pattern), where the arguments are wildcard binders for every argument except the one that corresponds to the field being accessed. 
  2. We use that pattern to construct a case expression that extracts the value corresponding to the field. 
    - This is kind of hard to explain without an example. Suppose we have `foo = {a: 1, b :: "yup"}` in `foo.b`
    - That gets turned into (something like) `case foo of {$GEN.Tuple2 _ $HERE -> $HERE} `

### Record Updates 
  1. Proceeds much like the accessor case, except we return a product in our case expression instead of returning a field. 
    - Using the above definition of `foo`, if we have `foo {b = "hello"}`, this turns into: 
    - `case foo of {$GEN.Tuple2 a _ -> $GEN.Tuple2 a "hello"}`
    
## Step 4: Final Conversion to PIR (Language.PureScript.CoreFn.Convert.ToPIR)

The final step of compilation is conceptually simple: We match on the constructors of our final IR and translate expressions (and the types they contain) into PIR `Term`s and `Type`s, using some machinery to generate fresh names when we need to construct a lambda (which should only happen when desugaring case expressions).

NOTE: The *implementation* of case expressions is incredibly complex. At the moment we support matching on simple constructor patterns. Going forward, we ought to weak the final IR so that case expressions are presented in a form that makes it simpler to handle. 




