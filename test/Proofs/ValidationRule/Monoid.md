# Proofs of Monoid laws for `forall e a. ValidationRule e a`

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
  (\x -> case (rl1 x, rl2 x) of
    (Failure e, _) -> Failure e                                         {- (i-a) -}
    (_, Failure e) -> Failure e                                         {- (i-b) -}
    _              -> Success ())                                       {- (i-c) -}

mempty :: ValidationRule e a
mempty = ValidationRule (\x -> Success ())                              {- (ii) -}
```

**Note**: The following proofs assume the `ValidationRule` semigroup instance is lawful.

## Identity law
> x <> mempty = x; forall x :: forall e a. ValidationRule e a
> mempty <> x = x; forall x :: forall e a. ValidationRule e a

### Case `x <> mempty = x`

**[L.H.S]**
```hs
=> x <> mempty
= ValidationRule rl <> mempty
= ValidationRule rl <> ValidationRule (\x -> Success ()) {- from (ii) -}
= ValidationRule                                         {- from (i) -}
    (\x -> case (rl x, (\x -> Success ()) x) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule
    (\x -> case (rl x, Success ()) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
------------------- /Possibilities/ -----------------------
= ValidationRule                                         {- 'rl x' evaluates to 'Success ()' -}
    (\x -> case (Success (), Success ()) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule (\x -> Success ())
= ValidationRule (\x -> rl x)
(OR)
= ValidationRule                                         {- 'rl x' evaluates to 'Failure e' -}
    (\x -> case (Failure e, Success ()) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule (\x -> Failure e)
= ValidationRule (\x -> rl x)
-----------------------------------------------------------
= ValidationRule (\x -> rl x)
= ValidationRule rl
```

**[R.H.S]**
```hs
=> x
= ValidationRule rl
```

<ins>L.H.S = R.H.S</ins>

### Case `mempty <> x = x`

**[L.H.S]**
```hs
=> mempty <> x
= mempty <> ValidationRule rl
= ValidationRule (\x -> Success ()) <> ValidationRule rl {- from (ii) -}
= ValidationRule                                         {- from (i) -}
    (\x -> case ((\x -> Success ()) x, rl x) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule
    (\x -> case (Success (), rl x) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
------------------- /Possibilities/ -----------------------
= ValidationRule                                         {- 'rl x' evaluates to 'Success ()' -}
    (\x -> case (Success (), Success ()) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule (\x -> Success ())
= ValidationRule (\x -> rl x)
(OR)
= ValidationRule                                         {- 'rl x' evaluates to 'Failure e' -}
    (\x -> case (Success (), Failure e) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule (\x -> Failure e)
= ValidationRule (\x -> rl x)
-----------------------------------------------------------
= ValidationRule (\x -> rl x)
= ValidationRule rl
```

**[R.H.S]**
```hs
=> x
= ValidationRule rl
```

Thus, Identity law is satisfied.
