module Valida () where

{-
const LoginData = object({ expires: number });

const UserData = object({
  username: string,
  password: string,
  login: option(string),
  sessions: map(string, LoginData),
  type: union(literal('a'), literal('b'), literal('c')),
});
-}

import Data.List.NonEmpty (NonEmpty ((:|)))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

-- | Like `Either`, but accumulates failures upon applicative composition.
data Validation e a = Failure e | Success a

instance Semigroup e => Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Semigroup e => Applicative (Validation e) where
    pure = Success
    (Success f) <*> (Success b)   = Success $ f b
    (Success _) <*> (Failure e)   = Failure e
    (Failure e) <*> (Success _)   = Failure e
    (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2

toEither :: Validation a b -> Either a b
toEither (Failure e) = Left e
toEither (Success a) = Right a

-- | The predicate a `Validator` uses to run validation.
data Predc e a
  -- | Builds a `Predc` from a function to generate error and a validation predicate.
  = Predc
  -- ^ Function taking the validation target and returning a value representing the error.
    (a -> e)
  -- ^ The validation predicate.
    (a -> Bool)

-- | Build a predicate that fails with given error if the given predicate succeeds.
failureIf :: (a -> Bool) -> e -> Predc (NonEmpty e) a
failureIf predicate err = Predc (const $ err :| []) $ not . predicate

-- | Like `failureIf` but uses Unit as the `Predc` error type.
failureIf' :: (a -> Bool) -> Predc () a
failureIf' = Predc (const ()) . (not .)

-- | Build a predicate that fails with given error unless the given predicate succeeds.
failureUnless :: (a -> Bool) -> e -> Predc (NonEmpty e) a
failureUnless predicate err = Predc (const $ err :| []) predicate

-- | Like `failureUnless` but uses Unit as the `Predc` error type.
failureUnless' :: (a -> Bool) -> Predc () a
failureUnless' = Predc (const ())

-- | Relabel a `Predc` with a new error generator.
label :: (a -> e) -> Predc x a -> Predc e a
label errF (Predc _ p) = Predc errF p

-- | A synonym for `label` with its arguments flipped.
infix 6 <?>

(<?>) :: Predc x a -> (a -> e) -> Predc e a
(<?>) = flip label

-- | An applicative validator. Validates a predicate on an input when run.
newtype Validator e inp a = Validator { validate :: inp -> Validation e a }

instance Semigroup e => Functor (Validator e inp) where
    -- | Run the validator function, and `fmap` given function over the result.
    fmap f (Validator v) = Validator $ (f <$>) . v

instance Semigroup e => Applicative (Validator e inp) where
    pure a = Validator (\_ -> Success a)
    (Validator ff) <*> (Validator v) = Validator $ (<*>) <$> ff <*> v

buildValidator :: Semigroup e => Predc e b -> Selector a b -> Validator e a b
buildValidator (Predc err predicate) selector = _validator
  where
    _validator = Validator
        $ \y -> let target = selector y
          in if predicate target
              then Success target
              else Failure $ err target

infix 5 -?-

(-?-) :: Semigroup e => Selector a b -> Predc e b -> Validator e a b
(-?-) = flip buildValidator

data Input = Input
    { inputName    :: String
    , inputAddress :: String
    }
  deriving (Eq, Show)

data InpValdError = NotUS | TooShort deriving (Show)

inputValidator :: Validator (NonEmpty InpValdError) Input Input
inputValidator = Input
    <$> buildValidator (failureIf (/="US") NotUS) inputAddress
    <*> buildValidator (failureIf ((<=3) . length) TooShort) inputName

inputValidator' :: Validator (NonEmpty InpValdError) Input Input
inputValidator' = Input
    <$> inputAddress -?- failureIf (/="US") NotUS
    <*> inputName -?- failureIf ((<=3) . length) TooShort

inputValidator'' :: Validator (NonEmpty InpValdError) Input Input
inputValidator'' = Input
    <$> inputAddress -?- failureIf' (/="US") <?> const (NotUS :| [])
    <*> inputName -?- failureIf' ((<=3) . length) <?> const (TooShort :| [])

inputValidator''' :: Validator (NonEmpty String) Input Input
inputValidator''' = Input
    <$> inputAddress -?- failureIf' (/="US") <?> (:|[]) . ("Expected: US; Got: " ++) . show
    <*> inputName -?- failureIf' ((<=3) . length) <?> (:|[]) . ("Name is too short: "++) . show
