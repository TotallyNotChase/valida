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
(<>) :: Semigroup e => Validator e inp a -> Validator e inp a -> Validator e inp a
Validator f <> Validator g = Validator (f <> g)                                    {- (i) -}
```

```hs
(<>) :: Semigroup b => (a -> b) -> (a -> b) -> (a -> b)
f <> g = \x -> f x <> g x                                                          {- (ii) -}
```

```hs
(<>) :: Semigroup e => Validation e a -> Validation e a -> Validation e a
Success a <> _         = Success a                                                 {- (iii) -}
_         <> Success a = Success a                                                 {- (iv) -}
Failure x <> Failure y = Failure (x <> y)                                          {- (v) -}
```

**Note**: The following proofs assume the `Validation` semigroup instance is lawful.

## Associativity law
> x <> (y <> z) = (x <> y) <> z; forall e inp a. Semigroup e => (x :: Validator e inp a), (y :: Validator e inp a), (z :: Validator e inp a)

**[L.H.S]**
```hs
=> x <> (y <> z)
= Validator a <> (Validator b <> Validator c)
= Validator a <> Validator (b <> c)           {- from (i) -}
= Validator (a <> (b <> c))                   {- from (i) -}
= Validator (a <> b <> c)                     {- since: (<>) is associative; from (ii) - (v) -}
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (Validator a <> Validator b) <> Validator c
= Validator (a <> b) <> Validator c           {- from (i) -}
= Validator ((a <> b) <> c)                   {- from (i) -}
= Validator (a <> b <> c)                     {- since: (<>) is associative; from (ii) - (v) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Associativity law is satisfied.
