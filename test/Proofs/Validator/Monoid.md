# Proofs of Monoid laws for `forall e inp. Validator e inp ()`

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
Validator f <> Validator g = Validator                                 {- (i) -}
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

mempty :: Validator e inp ()
mempty = Validator (\x -> Success ())                                  {- (ii) -}
```

Specializing `<>` to `Validator e inp ()`, we get-
```hs
(<>) :: Validator e inp () -> Validator e inp () -> Validator e inp ()
Validator f <> Validator g = Validator                                 {- (iii) -}
  (\x -> case f x of
    Failure e  -> Failure e
    Success () -> g x)
```

**Note**: The following proofs assume the `Validator` semigroup instance is lawful.

## Identity law
> x <> mempty = x; forall x :: forall e inp a. Monoid a => ValidationRule e a
> mempty <> x = x; forall x :: forall e inp a. Monoid a => ValidationRule e a

### Case `x <> mempty = x`

**[L.H.S]**
```hs
=> x <> mempty
= Validator v <> mempty
= Validator v <> Validator (\x -> Success ()) {- from (ii) -}
= Validator                                   {- from (iii) -}
    (\x' -> case v x' of
      Failure e  -> Failure e
      Success () -> ((\x -> Success ()) x'
    )
= Validator
    (\x' -> case v x' of
      Failure e  -> Failure e
      Success () -> Success ()
    )
= Validator (\x' -> v x')
= Validator v
```

**[R.H.S]**
```hs
=> x
= Validator v
```

<ins>L.H.S = R.H.S</ins>

### Case `mempty <> x = x`

**[L.H.S]**
```hs
=> mempty <> x
= mempty <> Validator v
= Validator (\x -> Success ()) <> Validator v {- from (ii) -}
= Validator                                   {- from (iii) -}
    (\x' -> case ((\x -> Success ()) x') of
      Failure e  -> Failure e
      Success () -> v x'
    )
= Validator
    (\x' -> case (Success ()) of
      Failure e  -> Failure e
      Success () -> v x'
    )
= Validator (\x' -> v x')
= Validator v
```

**[R.H.S]**
```hs
=> x
= Validator v
```

Thus, Identity law is satisfied.