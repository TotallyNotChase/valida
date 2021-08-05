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
  (\x -> case (rl1 x, rl2 x) of
    (Failure e, _) -> Failure e                                         {- (i-a) -}
    (_, Failure e) -> Failure e                                         {- (i-b) -}
    _              -> Success ())                                       {- (i-c) -}
```

## Associativity law
> x <> (y <> z) = (x <> y) <> z; forall e a. (x :: ValidationRule e a), (y :: ValidationRule e a), (z :: ValidationRule e a)

**[L.H.S]**
```hs
=> x <> (y <> z)
= ValidationRule rl1 <> (ValidationRule rl2 <> ValidationRule rl3)
= ValidationRule rl1 <> ValidationRule                             {- from (i) -}
    (\x -> case (rl2 x, rl3 x) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    )
= ValidationRule                                                   {- from (i) -}
    (\x -> case
      ( rl1 x
      , case (rl2 x, rl3 x) of
          (Failure e, _) -> Failure e
          (_, Failure e) -> Failure e
          _              -> Success ())
      ) of
        (Failure e, _) -> Failure e
        (_, Failure e) -> Failure e
        _              -> Success ()
    )
= ValidationRule                                                   {- Combine cases -}
    (\x -> case
      ( rl1 x
      , (rl2 x, rl3 x)
      ) of
        (Failure e, _) -> Failure e
        (_, (Failure e, _)) -> Failure e
        (_, (_, Failure e)) -> Failure e
        _              -> Success ()
    )
= ValidationRule                                                   {- Flatten case arguments -}
    (\x -> case (rl1 x, rl2 x, rl3 x) of
      (Failure e, _) -> Failure e
      (_, Failure e, _) -> Failure e
      (_, _, Failure e) -> Failure e
      _              -> Success ()
    )
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (ValidationRule rl1 <> ValidationRule rl2) <> ValidationRule rl3
= ValidationRule                                                   {- from (i) -}
    (\x -> case (rl1 x, rl2 x) of
      (Failure e, _) -> Failure e
      (_, Failure e) -> Failure e
      _              -> Success ()
    ) <> ValidationRule rl3
= = ValidationRule                                                   {- from (i) -}
    (\x -> case
      ( case (rl1 x, rl2 x) of
          (Failure e, _) -> Failure e
          (_, Failure e) -> Failure e
          _              -> Success ())
      , rl3 x
      ) of
        (Failure e, _) -> Failure e
        (_, Failure e) -> Failure e
        _              -> Success ()
    )
= ValidationRule                                                   {- Combine cases -}
    (\x -> case
      ( (rl1 x, rl2 x)
      , rl3 x
      ) of
        ((Failure e, _), _) -> Failure e
        ((_, Failure e), _) -> Failure e
        (_, Failure e) -> Failure e
        _              -> Success ()
    )
= ValidationRule                                                   {- Flatten case arguments -}
    (\x -> case (rl1 x, rl2 x, rl3 x) of
      (Failure e, _) -> Failure e
      (_, Failure e, _) -> Failure e
      (_, _, Failure e) -> Failure e
      _              -> Success ()
    )
=
```

<ins>L.H.S = R.H.S</ins>

Thus, Associativity law is satisfied.
