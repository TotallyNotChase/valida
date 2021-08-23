# Proofs of Semigroup laws for `forall e inp a. Validator e inp a`

```hs
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a }
```
where-
```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
(<>) :: Validator e inp a -> Validator e inp a -> Validator e inp a
Validator f <> Validator g = Validator                              {- (i) -}
  (\x -> case f x of
    Failure e -> Failure e
    Success _ -> g x)
{- Simplified -}

{- Original version:-

Validator v1 <> Validator v2 = Validator
  (\x -> case (v1 x, v2 x) of
    (Failure e, _) -> Failure e
    (_, b) -> b)
-}
```

## Associativity law
> x <> (y <> z) = (x <> y) <> z; forall e inp a. => (x :: Validator e inp a), (y :: Validator e inp a), (z :: Validator e inp a)

**[L.H.S]**
```hs
=> x <> (y <> z)
= Validator a <> (Validator b <> Validator c)
= Validator a <> Validator                    {- from (i) -}
    (\x -> case b x of
      Failure e -> Failure e
      Success _ -> c x
    )
= Validator                                   {- from (i) -}
    (\x' -> case a x' of
      Failure e -> Failure e
      Success _ -> (\x -> case b x of
        Failure e -> Failure e
        Success _ -> c x) x'
    )
= Validator
    (\x' -> case a x' of
      Failure e -> Failure e
      Success _ -> case b x' of
        Failure e -> Failure e
        Success _ -> c x'
    )
= Validator
    (\x -> case a x of
      Failure e -> Failure e
      Success _ -> case b x of
        Failure e  -> Failure e
        Success _ -> c x
    )
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (Validator a <> Validator b) <> Validator c
= Validator                                   {- from (i) -}
    (\x -> case a x of
      Failure e -> Failure e
      Success _ -> b x
    ) <> Validator c
= Validator                                   {- from (i) -}
    (\x' ->
      case (
        (\x -> case a x of
          Failure e -> Failure e
          Success _ -> b x
        ) x'
      ) of
        Failure e -> Failure e
        Success _ -> c x'
    )
= Validator
    (\x' ->
      case (
        case a x' of
          Failure e -> Failure e
          Success _ -> b x'
      ) of
        Failure e -> Failure e
        Success _ -> c x'
    )
= Validator
    (\x' -> case a x' of
      Failure e -> Failure e
      Success _ -> case b x' of
        Failure e -> Failure e
        Success _ -> c x'
    )
= Validator
    (\x -> case a x of
      Failure e -> Failure e
      Success _ -> case b x of
        Failure e -> Failure e
        Success _ -> c x
    )
```

<ins>L.H.S = R.H.S</ins>

Thus, Associativity law is satisfied.
