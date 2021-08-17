# Proofs of Semigroup laws for `forall e a. ValidationRule e a`

```hs
newtype ValidationRule e a = ValidationRule (a -> Validation e ())
```
where-
```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
(<>) :: ValidationRule e a -> ValidationRule e a -> ValidationRule e a
ValidationRule rl1 <> ValidationRule rl2 = ValidationRule               {- (i) -}
  (\x -> case rl1 x of
    Failure e  -> Failure e
    Success () -> rl2 x)
{- __Simplified__ -}

{- Original version:-

ValidationRule rl1 <> ValidationRule rl2 = ValidationRule
  (\x -> case (rl1 x, rl2 x) of
    (Failure e, _) -> Failure e
    (_, b) -> b)
-}
```

## Associativity law
> x <> (y <> z) = (x <> y) <> z; forall e a. (x :: ValidationRule e a), (y :: ValidationRule e a), (z :: ValidationRule e a)

**[L.H.S]**
```hs
=> x <> (y <> z)
= ValidationRule rl1 <> (ValidationRule rl2 <> ValidationRule rl3)
= ValidationRule rl1 <> ValidationRule                             {- from (i) -}
    (\x -> case rl2 x of
      Failure e  -> Failure e
      Success () -> rl3 x)
= ValidationRule                                                   {- from (i) -}
    (\x' -> case rl1 x' of
      Failure e  -> Failure e
      Success () -> (\x -> case rl2 x of
        Failure e  -> Failure e
        Success () -> rl3 x) x'
    )
= ValidationRule
    (\x' -> case rl1 x' of
      Failure e  -> Failure e
      Success () -> case rl2 x' of
        Failure e  -> Failure e
        Success () -> rl3 x'
    )
= ValidationRule
    (\x -> case rl1 x of
      Failure e  -> Failure e
      Success () -> case rl2 x of
        Failure e  -> Failure e
        Success () -> rl3 x
    )
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (ValidationRule rl1 <> ValidationRule rl2) <> ValidationRule rl3
= ValidationRule                                                   {- from (i) -}
    (\x -> case rl1 x of
      Failure e  -> Failure e
      Success () -> case rl2 x of
        Failure e  -> Failure e
        Success () -> Success ()
    ) <> ValidationRule rl3
= ValidationRule                                                   {- from (i) -}
    (\x' ->
      case (
        (\x -> case rl1 x of
          Failure e  -> Failure e
          Success () -> rl2 x
        ) x'
      ) of
        Failure e  -> Failure e
        Success () -> rl3 x'
    )
= ValidationRule
    (\x' ->
      case (
        case rl1 x' of
          Failure e  -> Failure e
          Success () -> rl2 x'
      ) of
        Failure e  -> Failure e
        Success () -> rl3 x'
    )
= ValidationRule
    (\x' -> case rl1 x' of
      Failure e  -> Failure e
      Success () -> case rl2 x' of
        Failure e  -> Failure e
        Success () -> rl3 x'
    )
= ValidationRule
    (\x -> case rl1 x of
      Failure e  -> Failure e
      Success () -> case rl2 x of
        Failure e  -> Failure e
        Success () -> rl3 x
    )
```

<ins>L.H.S = R.H.S</ins>

Thus, Associativity law is satisfied.
