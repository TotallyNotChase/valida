# Valida
Simple, elegant, applicative validation for product types - batteries included!

Read the documentation on [hackage](https://hackage.haskell.org/package/valida).

# Highlights
* Minimal - *Zero dependencies* apart from `base`.
* Batteries included - `ValidationRule` combinators for almost every scenario.
* Validation without the boiler plate - a highly specialized usage of contravariant functors to conveniently model the common validation usecases, without extra boilerplate.

# Quick Taste
```hs
import Data.List.NonEmpty (NonEmpty)

import Valida

data InputForm = InpForm
  { inpName  :: String
  , inpAge   :: Int
  , inpEmail :: Maybe String
  } deriving (Show)

data ValidInput = ValidInput
  { vInpName  :: String
  , vInpAge   :: Int
  , vInpEmail :: Maybe String
  } deriving (Show)

data FormErr
  = InvalidNameLength
  | InvalidAge
  | NoAtCharInMail
  | NoPeriodInMail
  | InvalidEmailLength
  deriving (Show)

-- | Validator for each field in the input form - built using 'ValidationRule' combinators.
inpFormValidator :: Validator (NonEmpty FormErr) InputForm ValidInput
inpFormValidator = ValidInput
    -- Name should be between 1 and 20 characters long
    <$> inpName -?> lengthWithin (1, 20) InvalidNameLength
    -- Age should be between 18 and 120
    <*> inpAge -?> valueWithin (18, 120) InvalidAge
    -- Email, if provided, should contain '@', and '.', and be atleast 5 characters long
    <*> inpEmail -?> optionally (minLengthOf 5 InvalidEmailLength
        <> mustContain '@' NoAtCharInMail
        <> mustContain '.' NoPeriodInMail)

goodInput :: InputForm
goodInput = InpForm "John Doe" 42 Nothing

badInput :: InputForm
badInput = InpForm "John Doe" 17 (Just "@")

main :: IO ()
main = do
    print (runForm inpFormValidator goodInput)
    -- Prints- Success (ValidInput {vInpName = "John Doe", vInpAge = 42, vInpEmail = Nothing})
    print (runForm inpFormValidator badInput)
    -- Prints- Failure (InvalidAge :| [InvalidEmailLength])
```

You can also find more examples [here](./examples/Main.hs).

# Quick Start
The primary purpose of the `Validator` type is to validate each field in product types. To do this, you'll use [`verify`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:verify).

`verify` takes 2 inputs-
* The "selector", which essentially just takes the product type as input, and returns the specific value of the specific field to validate.
* The `ValidationRule`, which specifies the predicate the field must satisfy - as well as the error value to yield if it doesn't satisfy said predicate

Let's validate a pair for example, the first field should be an int less than 10, the second field should be a non empty string. Then, the validator would look like-
```hs
pairValidator :: Validator (NonEmpty String) (Int, String) (Int, String)
pairValidator = (,) <$> verify (failureIf (>=10) "NotLessThan10") fst <*> verify (notEmpty "EmptyString") snd
```
Or, if you prefer using operators - you can use [`-?>`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:-45--63--62-), which is a flipped version of `verify`.
```hs
pairValidator :: Validator (NonEmpty String) (Int, String) (Int, String)
pairValidator = (,)
    <$> fst -?> failureUnless (<10) "NotLessThan10"
    <*> snd -?> notEmpty "EmptyString"
```

You can then run the validator on your input using [`runValidator`](https://hackage.haskell.org/package/valida/docs/Valida.html#t:Validator)-
```hs
>>> runValidator pairValidator (9, "foo")
Success (9,"foo")
>>> runValidator pairValidator (10, "")
Failure ("NotLessThan10" :| ["EmptyString"])
>>> runValidator pairValidator (5, "")
Failure ("EmptyString" :| [])
```

This is the core concept for building the validators. You can use the primitive combinators (e.g [`failureIf`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureIf), [`failureUnless`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureUnless)) to build `ValidationRule`s directly from predicate functions, or you can choose one of the many derivate combinators (e.g [`notEmpty`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:notEmpty)) to build `ValidationRule`s. Check out the [`Valida.Combinators`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html) module documentation to view all the included combinators.

## Validators for non product types
Although the primary purpose of Valida is building convenient validators for product types. Sometimes, you'll find yourself not needing to select on any field, but validating the input directly. In that case, you may find yourself using this pattern-
```hs
-- | Make sure int input is not even.
intValidator :: Validator (NonEmpty String) Int Int
intValidator = verify (failureIf even "Even") id
```

In these situations, instead of using `verify` with `id` as selector, you should use [`validate`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:validate) instead, which is the same as `flip verify id`-
```hs
intValidator :: Validator (NonEmpty String) Int Int
intValidator = validate (failureIf even "Even")
```

## Combining multiple `ValidationRule`s
Often, you'll find yourself in situations where you expect the input to satisfy *multiple* `ValidationRule`s, or situations where you expect the input to satisfy *at least one* of many `ValidationRule`s. This is where [`andAlso`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:andAlso), and [`orElse`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:orElse) come into play.

### Combining multiple `ValidationRule`s with `andAlso`
`andAlso` is the semigroup implementation of `ValidationRule`, and thus is the same as `<>`. Combining 2 rules with `<>` creates a new rule that is only satisfied when *both of the given rules are satisfied*. Otherwise, the very first (left most) failure value is returned - and the rest are not tried.

The following rule only succeeds if the input is **odd**, *and* **not divisble by 3**.
```hs
rule :: ValidationRule (NonEmpty String) Int
rule = failureIf even "IsEven" `andAlso` failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```
(OR)
```hs
rule :: ValidationRule (NonEmpty String) Int
rule = failureIf even "IsEven" <> failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```

Usages-
```hs
>>> runValidator (validate rule) 5
Success 5
>>> runValidator (validate rule) 4
Failure ("IsEven" :| [])
>>> runValidator (validate rule) 15
Failure ("IsDivisbleBy3" :| [])
>>> runValidator (validate rule) 6
Failure ("IsEven" :| [])
```

### Combining multiple `ValidationRule`s with `orElse`
`orElse` also forms a semigroup, `</>` is aliased to `orElse`. Combining 2 rules with `</>` creates a new rule that is satisfied when *either of the given rules are satsified*. If all of them fail, the `Failure` values are accumulated.

The following rule succeeds if the input is *either* **odd**, *or* **not divisble by 3**.
```hs
rule :: ValidationRule (NonEmpty String) Int
rule = failureIf even "IsEven" `orElse` failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```
(OR)
```hs
rule :: ValidationRule (NonEmpty String) Int
rule = failureIf even "IsEven" </> failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```

Usages-
```hs
>>> runValidator (validate rule) 5
Success 5
>>> runValidator (validate rule) 4
Success 4
>>> runValidator (validate rule) 15
Success 15
>>> runValidator (validate rule) 6
Failure ("IsEven" :| ["IsDivisbleBy3"])
```

### Combining a foldable of `ValidationRule`s
You can combine a foldable of `ValidationRule`s using [`satisfyAll`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:satisfyAll) and [`satisfyAny`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:satisfyAny). `satisfyAll` folds using `andAlso`/`<>`, while `satisfyAny` folds using `orElse`/`</>`.

## Ignoring errors
Although, highly inadvisable and generally not useful in serious code, you may use alternative versions of `ValidationRule` combinators that use `()` (unit) as its error type so you don't have to supply error values. For example, [`failureIf'`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureIf-39-) does not require an error value to be supplied. In case of failure, it simply yields `Failure ()`.
```hs
>>> runValidator (validate (failureIf' even)) 2
Failure ()
```

## Re-assigning errors
Using the [`label`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:label)/[`<?>`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:-60--63--62-) and [`labelV`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:labelV)/[`<??>`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:-60--63--63--62-) functions, you can use override the errors `ValidationRule`s and `Validator`s yield.

For example, to re assign the error on a `ValidationRule`-
```hs
label "IsEven" (failureIf even "Foo")
```
(OR)
```hs
failureIf even "Foo" <?> "IsEven"
```

This is useful with `ValidationRule`s that use unit as their error type. You can create a `ValidationRule`, skip assigning an error to it - and label a specific error when you need it later.
```hs
label "IsEven" (failureIf' even)
```

Re-labeled `ValidationRule`s will yield the newly assigned error value when the rule is not satisfied.

Similarly, `labelV` (or `<??>`) can be used to relabel the error value of an entire `Validator`.

## Wait, couldn't this be done using contravariant functors?
Yes! The concept of *keeping the input of a `Validator`* set to the same product type, but *letting it validate a specific field* of said input, can be generalized to contravariant functors. The `Validator` type looks like- `Validator e inp a`, to keep applicative composition working, the `inp` needs to stay the same - but each validator within said composition should also be able to *consume* a specific part of the `inp`. `ValidationRule` itself, is the typical example of a contravariant functor as well. It's essentially a specialized predicate function- `a -> Validation e ()`. The `verify` function simply combines these 2 *potentially generalizable* contravariant functors, into a very specialized usecase.

I do think adding instances for actual generalized contravariant functors/profunctors for `ValidationRule` and `Validator`, *could* be more powerful, while also being able to provide the same specialized functions. However, I've refrained from doing so as I didn't want to pull in the extra dependencies. I think the separation of `ValidationRule` and `Validator`, combined with the provided functions, *should* be able to reliably, and elegantly model any scenario of building a validator. I have yet to find a usecase where the generalized contravariant instances *would be significantly useful*. But it could certainly be more idiomatic for haskell. I may consider creating a package that *does* go the contravariant/profunctor route though.

# Comparison and Motivation
The concept of the `Validation` data type used in this package isn't new. It's also used in the following packages-
* [either](https://hackage.haskell.org/package/either)
* [validation](https://hackage.haskell.org/package/validation)
* [validation-selective](https://hackage.haskell.org/package/validation-selective)

Valida aims to be a minimal in terms of dependencies, but batteries included in terms of API. It borrows many philosophies from `Data.Validation` (from `validation`) and `Validation` (from `validation-selective`), and aims to provide a convenient, minimal way to model the common usecases of them.

The `verify` function, combined with the `ValidationRule` combinators, and the parsec-esque `Validator` aims to assist in easily modeling typical validation usecases without too much boilerplate. The core idea, really, is [contravariance](#wait-couldnt-this-be-done-using-contravariant-functors) - the typical usecases, especially when validating product types (the most common target of validation), simply showcases contravariant functors.

In essence, the validation style itself, is designed to look like [forma](https://hackage.haskell.org/package/forma). Though the actual types, and core concepts are significantly different.
