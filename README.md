# Valida
Simple, elegant, profunctorial, applicative validation for product types - batteries included!

Read the documentation on [hackage](https://hackage.haskell.org/package/valida).

# Highlights
* Minimal - **Singular** external dependency: profunctors.

  If you'd like a more lightweight version. Checkout [valida-base](https://hackage.haskell.org/package/valida-base), which offers similar functionalites *without **any** external dependency*.
* Batteries included - `Validator` combinators for almost every scenario.
* Validation without the boiler plate - Implementation of contravariance to conveniently model the common validation usecases, without extra boilerplate.
* Profunctorial, Applicative Validator - Relating to the previous point, the provided `Validator` type is not only an applicative functor, but also a profunctor. This is what allows the contravariance on its input argument.

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

-- | Validator for each field in the input form - built using 'Validator' combinators.
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
    print (runValidator inpFormValidator goodInput)
    -- Prints- Success (ValidInput {vInpName = "John Doe", vInpAge = 42, vInpEmail = Nothing})
    print (runValidator inpFormValidator badInput)
    -- Prints- Failure (InvalidAge :| [InvalidEmailLength])
```

You can also find more examples [here](./examples/Main.hs).

# Quick Start
The primary purpose of the `Validator` type is to validate each field in product types. To do this, you'll use [`verify`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:verify).

`verify` takes 2 inputs-
* The "selector", which essentially just takes the product type as input, and returns the specific value of the specific field to validate.
* The `Validator`, which specifies the predicate the field must satisfy, the error value to yield if it doesn't satisfy said predicate, and the output upon successful validation.

Let's validate a pair for example, the first field should be an int less than 10, the second field should be a non empty string. Then, the validator would look like-
```hs
pairValidator :: Validator (NonEmpty String) (Int, String) (Int, String)
pairValidator = (,) <$> verify (failureIf (>=10) "NotLessThan10") fst <*> verify (notEmpty "EmptyString") snd
```
Or, if you prefer using operators - you can use [`-?>`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:-45--63--62-), which is a flipped version of `verify`.
```hs
pairValidator :: Validator (NonEmpty String) (Int, String) (Int, String)
pairValidator = (,)
    <$> fst -?> failureIf (>=10) "NotLessThan10"
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

This is the core concept for building the validators. You can use the primitive combinators (e.g [`failureIf`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureIf), [`failureUnless`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureUnless)) to build `Validator`s directly from predicate functions, or you can choose one of the many derivate combinators (e.g [`notEmpty`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:notEmpty)) to build `Validator`s. Check out the [`Valida.Combinators`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html) module documentation to view all the included combinators.

## Combining multiple `Validator`s
Often, you'll find yourself in situations where you expect the input to satisfy *multiple* `Validator`s (but don't need applicative composition), or situations where you expect the input to satisfy *at least one* of multiple `Validator`s. This is where [`andAlso`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:andAlso), and [`orElse`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:orElse) come into play.

### Combining multiple `Validator`s with `andAlso`
`andAlso` is the semigroup implementation of `Validator`, and thus is the same as `<>`. Combining 2 validators with `<>` creates a new validator that is only satisfied when *both of the given validators are satisfied*.

Otherwise, the **first (left most)** failure value is returned - and the rest are not tried. Upon successful validation, the **right-most** `Success` value is returned. This means that if all validators succeed, only the right-most validator's success value is returned.

The following validator only succeeds if the input is **odd**, *and* **not divisble by 3**.
```hs
validator :: Validator (NonEmpty String) Int Int
validator = failureIf even "IsEven" `andAlso` failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```
(OR)
```hs
validator :: Validator (NonEmpty String) Int Int
validator = failureIf even "IsEven" <> failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```

Usages-
```hs
>>> runValidator validator 5
Success 5
>>> runValidator validator 4
Failure ("IsEven" :| [])
>>> runValidator validator 15
Failure ("IsDivisbleBy3" :| [])
>>> runValidator validator 6
Failure ("IsEven" :| [])
```

### Combining multiple `Validator`s with `orElse`
`orElse` also forms a semigroup, `</>` is aliased to `orElse`. Combining 2 validators with `</>` creates a new validator that is satisfied when *either of the given validators are satsified*. If all of them fail, the `Failure` values are **accumulated**. The **left-most** `Success` value is returned, remaining validators are not tried.

The following validator succeeds if the input is *either* **odd**, *or* **not divisble by 3**.
```hs
validator :: Validator (NonEmpty String) Int Int
validator = failureIf even "IsEven" `orElse` failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```
(OR)
```hs
validator :: Validator (NonEmpty String) Int Int
validator = failureIf even "IsEven" </> failureIf ((==0) . flip mod 3) "IsDivisbleBy3"
```

Usages-
```hs
>>> runValidator validator 5
Success 5
>>> runValidator validator 4
Success 4
>>> runValidator validator 15
Success 15
>>> runValidator validator 6
Failure ("IsEven" :| ["IsDivisbleBy3"])
```

### Combining a foldable of `Validator`s
You can combine a foldable of `Validator`s using [`satisfyAll`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:satisfyAll) and [`satisfyAny`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:satisfyAny). `satisfyAll` folds using `andAlso`/`<>`, while `satisfyAny` folds using `orElse`/`</>`.

## Ignoring errors
Although, highly inadvisable and generally not useful in serious code, you may use alternative versions of `Validator` combinators that use `()` (unit) as the error type so you don't have to supply error values. For example, [`failureIf'`](https://hackage.haskell.org/package/valida/docs/Valida-Combinators.html#v:failureIf-39-) does not require an error value to be supplied. In case of failure, it simply yields `Failure ()`.
```hs
>>> runValidator (failureIf' even) 2
Failure ()
```

## Re-assigning errors
Using the [`label`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:label)/[`<?>`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:-60--63--62-) function, you can override the errors `Validator`s yield.

For example, to re assign the error on a `Validator`-
```hs
label "IsEven" (failureIf even "Foo")
```
(OR)
```hs
failureIf even "Foo" <?> "IsEven"
```

This is useful with `Validator`s that use unit as their error type. You can create a `Validator`, skip assigning an error to it - and label a specific error when you need to later.
```hs
label "IsEven" (failureIf' even)
```

Re-labeled `Validator`s will yield the newly assigned error value when the validator is not satisfied.

# Core Idea
All usecases of applicative validation, involving validation of product types, have one noticable thing in common. A well written validator typically looks something like-
```hs
data InputForm = InpForm
  { inpName :: String
  , inpAge  :: Int
  , inpDate :: String
  } deriving (Show)

validateName :: String -> Validation [String] String
validateAge  :: Int -> Validation [String] Int
validateDate :: String -> Validation [String] String

validateForm :: InputForm -> Validation [String] InputForm
validateForm form = InputForm
  <$> validateName (inpName form)
  <*> validateAge (inpAge form)
  <*> validateDate (inpDate form)
```

There's a few things unideal with this. The functions `validateName`, `validateAge`, and `validateDate` are defined elsewhere - but all of their definitions are really similar. Yet, without handy combinators - they can't be defined in a terse way. However, the bigger problem, is how all of the validators need to be fed their specific input by selecting the field from the product type. It could look better if the validator functions could somehow just be linked to a specific field selector in an elegant way. Something like `inpName -?> validateName`, perhaps.

This is the perfect usecase for contravariance. A validation function, is really just a [`Predicate`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor-Contravariant.html#t:Predicate), the idiomatic example of a contravariant functor. However, it *also* needs to be an applicative functor to allow for the elegant composition. In fact, the type of a validation function needs to parameterize on 3 types - `inp -> Validation e a`
* The input type
* The error type
* The output type

The output is covariant, but the input is contravariant. This is a [Profunctor](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html)! With a profunctorial validator, you now have the ability to *not only* map the output type, *but also* contramap the input type.

Given a validator that makes sure an int input is even, and returns said int input as output - `evenValidator`, you can *easily* use it in the applicative validation of a `(Int, Int)` using `lmap`-
```hs
(,) <$> lmap fst evenValidator <*> lmap snd evenValidator
```

Contravariant input, mixed with covariant output - is the bread and butter of Valida! It allows for elegant encoding of well composable validators using only 2 simple concepts.

There's one more core idea that `Valida` uses though - [`fixV`](https://hackage.haskell.org/package/valida/docs/Valida.html#v:fixV). `fixV` "fixes" a validator's output, to be the same as its input. `fmap` lets you *map* over the output, `lmap` lets you *contramap* over the input, `fixV` allows `fmap` to now *map* over the input value, on the output position. `fixV` also allows you to regain the input value in the output position if a validator has been `fmap`ed on.

# Comparison and Motivation
The concept of the `Validation` data type used in this package isn't new. It's also used in the following packages-
* [either](https://hackage.haskell.org/package/either)
* [validation](https://hackage.haskell.org/package/validation)
* [validation-selective](https://hackage.haskell.org/package/validation-selective)

Valida aims to be a minimal in terms of dependencies, but batteries included in terms of API. It borrows many philosophies from `Data.Validation` (from `validation`) and `Validation` (from `validation-selective`), and aims to provide a convenient, minimal way to model the common usecases of them.

The `verify` function, combined with the built in `Validator` combinators, and the parsec-esque `Validator` aims to assist in easily modeling typical validation usecases without too much boilerplate, using applicative composition and contravariant input.

In essence, the validation style itself, is designed to look like [forma](https://hackage.haskell.org/package/forma). Though the actual types, and core concepts are significantly different.
