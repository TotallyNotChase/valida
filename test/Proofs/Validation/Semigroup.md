# Proofs of Semigroup laws for `forall e a. Semigroup e => Validation e a`

```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
(<>) :: Semigroup e => Validation e a -> Validation e a -> Validation e a
Success a <> _         = Success a                                         {- (i) -}
_         <> Success a = Success a                                         {- (ii) -}
Failure x <> Failure y = Failure (x <> y)                                  {- (iii) -}
```

## Associativity law
> x <> (y <> z) = (x <> y) <> z; forall e a. Semigroup e => (x :: Validation e a), (y :: Validation e a), (z :: Validation e a)

### Case a - All three of `x, y, z` are `forall e. Semigroup e => Failure e`

**Note**: Denoting the three `forall e. Semigroup e => Failure e` as `Failure a`, `Failure b`, and `Failure c` respectively.

**[L.H.S]**
```hs
=> x <> (y <> z)
= Failure a <> (Failure b <> Failure c)
= Failure a <> Failure (b <> c)          {- from (iii) -}
= Failure (a <> (b <> c))                {- from (iii) -}
= Failure (a <> b <> c)                  {- since: (<>) is associative -}
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (Failure a <> Failure b) <> Failure c
= Failure (a <> b) <> Failure c          {- from (iii) -}
= Failure ((a <> b) <> c)                {- from (iii) -}
= Failure (a <> b <> c)                  {- since: (<>) is associative -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - Exactly one of `x, y, z` is `forall a. Success a`

**[L.H.S]**
```hs
=> x <> (y <> z)
------------------- /Possibilities/ -----------------------
= Success a <> (Failure _ <> Failure _)
(OR)
= Failure _ <> (Success a <> Failure _)
(OR)
= Failure _ <> (Failure _ <> Success a)
-----------------------------------------------------------
= Success a                              {- from (i) and (ii) -}
```

**[R.H.S]**
```hs
=> (x <> y) <> z
------------------- /Possibilities/ -----------------------
= (Success a <> Failure _) <> Failure _
(OR)
= (Failure _ <> Success a) <> Failure _
(OR)
= (Failure _ <> Failure _) <> Success a
-----------------------------------------------------------
= Success a                              {- from (i) and (ii) -}
```

<ins>L.H.S = R.H.S</ins>

### Case c - Exactly two of `x, y, z` are `forall a. Success a`

**Note**: Denoting the first of such `forall a. Success a` as `Success a` and the other as `Success b`.

**[L.H.S]**
```hs
=> x <> (y <> z)
------------------- /Possibilities/ -----------------------
= Success a <> (Success b <> Failure _)
(OR)
= Failure _ <> (Success a <> Success b)
(OR)
= Success a <> (Failure _ <> Success b)
-----------------------------------------------------------
= Success a                              {- from (i) and (ii) -}
```

**[R.H.S]**
```hs
=> (x <> y) <> z
------------------- /Possibilities/ -----------------------
= (Success a <> Success b) <> Failure _
(OR)
= (Failure _ <> Success a) <> Success b
(OR)
= (Success a <> Failure _) <> Success b
-----------------------------------------------------------
= Success a                              {- from (i) and (ii) -}
```

<ins>L.H.S = R.H.S</ins>

### Case d - All three of `x, y, z` are `forall a. Success a`

**Note**: Denoting the three `forall a. Success a` as `Success a`, `Success b` and `Success c` respectively.

**[L.H.S]**
```hs
=> x <> (y <> z)
= Success a <> (Success b <> Success c)
= Success a <> Success b                 {- from (i) -}
= Success a                              {- from (i) -}
```

**[R.H.S]**
```hs
=> (x <> y) <> z
= (Success a <> Success b) <> Success c
= Success a <> Success c                 {- from (i) -}
= Success a                              {- from (i) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Associativity law is satisfied.
