# Proofs of Applicative Functor laws for `forall e a. Semigroup e => Validation e a`

```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
pure :: a -> Validation e a
pure = Success                                                                    {- (i) -}

(<*>) :: Semigroup e => Validation e (a -> b) -> Validation e a -> Validation e b
Success f <*> Success b = Success (f b)                                           {- (ii) -}
Success _ <*> Failure e = Failure e                                               {- (iii) -}
Failure e <*> Success _ = Failure e                                               {- (iv) -}
Failure x <*> Failure y = Failure (x <> y)                                        {- (v) -}

fmap :: (a -> b) -> Validation e a -> Validation e b
fmap f (Failure e) = Failure e                                                    {- (vi) -}
fmap f (Success a) = Success (f a)                                                {- (vii) -}
```

## Identity law
> pure id <\*> v = v; forall v :: forall e a. Semigroup e => Validation e a

### Case a - `v = forall e. Semigroup e => Failure e`

**[L.H.S]**
```hs
=> pure id <*> v
= pure id <*> Failure e
= Success id <*> Failure e                              {- from (i) -}
= Failure e                                             {- from (iii) -}
```

**[R.H.S]**
```hs
=> v
= Failure e
```

<ins>L.H.S = R.H.S</ins>

### Case b - `v = forall a. Success a`

**[L.H.S]**
```hs
=> pure id <*> v
= pure id <*> Success a
= Success id <*> Success a                              {- from (i) -}
= Success (id a)                                        {- from (ii) -}
= Success a                                             {- since: id a = a -}
```

**[R.H.S]**
```hs
=> v
= Success a
```

<ins>L.H.S = R.H.S</ins>

Thus, Identity law is satisfied.

## Composition law
> pure (.) <\*> u <\*> v <\*> w = u <\*> (v <\*> w); forall e a b c. (Semigroup e, Eq c) => (w :: Validation e a), (u :: Validation e (b -> c)), (v :: Validation e (a -> b))

### Case a - `forall a b c. Eq c => u = Success (f :: b -> c), v = Success (g :: a -> b), w = Success a`

**[L.H.S]**
```hs
=> pure (.) <*> u <*> v <*> w
= pure (.) <*> Success f <*> Success g <*> Success a
= Success (.) <*> Success f <*> Success g <*> Success a {- from (i) -}
= Success ((.) f) <*> Success g <*> Success a           {- from (ii) -}
= Success ((.) f g) <*> Success a                       {- from (ii) -}
= Success (\x -> f (g x)) <*> Success a                 {- since: f . g = \x -> f (g x) -}
= Success ((\x -> f (g x)) a)                           {- from (ii) -}
= Success (f (g a))
```

**[R.H.S]**
```hs
=> u <*> (v <*> w)
= Success f <*> (Success g <*> Success a)
= Success f <*> Success (g a)                           {- from (ii) -}
= Success (f (g a))                                     {- from (ii) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - Exactly one of `u, v, w` is `forall e. Semigroup e => Failure e`

**[L.H.S]**
```hs
=> pure (.) <*> u <*> v <*> w
= Success (.) <*> u <*> v <*> w                         {- from (i) -}
------------------- /Possibilities/ -----------------------
= Success (.) <*> Failure e <*> Success _ <*> Success _
(OR)
= Success (.) <*> Success _ <*> Failure e <*> Success _
(OR)
= Success (.) <*> Success _ <*> Success _ <*> Failure e
-----------------------------------------------------------
= Failure e                                             {- from (iii) and (iv) -}
```

**[R.H.S]**
```hs
=> u <*> (v <*> w)
------------------- /Possibilities/ -----------------------
= Failure e <*> (Success _ <*> Success _)
(OR)
= Success _ <*> (Failure e <*> Success _)
(OR)
= Success _ <*> (Success _ <*> Failure e)
-----------------------------------------------------------
= Failure e                                             {- from (iii) and (iv) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - Exactly two of `u, v, w` is `forall e. Semigroup e => Failure e`

**Note**: Denoting the first of such `forall e. Semigroup e => Failure e` as `Failure x` and the other as `Failure y`.

**[L.H.S]**
```hs
=> pure (.) <*> u <*> v <*> w
= Success (.) <*> u <*> v <*> w                         {- from (i) -}
------------------- /Possibilities/ -----------------------
= Success (.) <*> Failure x <*> Failure y <*> Success _
(OR)
= Success (.) <*> Success _ <*> Failure x <*> Failure y
(OR)
= Success (.) <*> Failure x <*> Success _ <*> Failure y
-----------------------------------------------------------
= Failure (x <> y)                                      {- from (iii), (iv), and (v) -}
```

**[R.H.S]**
```hs
=> u <*> (v <*> w)
------------------- /Possibilities/ -----------------------
= Failure x <*> (Failure y <*> Success _)
(OR)
= Success _ <*> (Failure x <*> Failure y)
(OR)
= Failure x <*> (Success _ <*> Failure y)
-----------------------------------------------------------
= Failure (x <> y)                                      {- from (iii), (iv), and (v) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - All three of `u, v, w` is `forall e. Semigroup e => Failure e`

**Note**: Denoting the three `forall e. Semigroup e => Failure e` as `Failure x`, `Failure y`, and `Failure z` respectively.

**[L.H.S]**
```hs
=> pure (.) <*> u <*> v <*> w
= pure (.) <*> Failure x <*> Failure y <*> Failure z
= Success (.) <*> Failure x <*> Failure y <*> Failure z {- from (i) -}
= Failure x <*> Failure y <*> Failure z                 {- from (iii) -}
= Failure (x <> y) <*> Failure z                        {- from (v) -}
= Failure ((x <> y) <> z)                               {- from (v) -}
= Failure (x <> y <> z)                                 {- since: (<>) is associative -}
```

**[R.H.S]**
```hs
=> Failure x <*> (Failure y <*> Failure z)
= Failure x <*> Failure (y <> z)                        {- from (v) -}
= Failure (x <> (y <> z))                               {- from (v) -}
= Failure (x <> y <> z)                                 {- since: (<>) is associative -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Composition law is satisfied.

## Homomorphism law
> pure f <\*> pure x = pure (f x); forall a b. (x :: a), (f :: a -> b)

**[L.H.S]**
```hs
=> pure f <*> pure x
= Success f <*> Success x                               {- from (i) -}
= Success (f x)                                         {- from (ii) -}
```

**[R.H.S]**
```hs
=> pure (f x)
= Success (f x)                                         {- from (i) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Homomorphism law is satisfied.

## Interchange law
> u <\*> pure y = pure ($ y) <\*> u; forall a b. (y :: a), (u :: forall e. Semigroup e => Validation e (a -> b))

### Case a - `u = forall e. Semigroup e => Failure e`

**[L.H.S]**
```hs
=> u <*> pure y
= Failure e <*> pure y
= Failure e <*> Success y                               {- from (i) -}
= Failure e                                             {- from (iv) -}
```

**[R.H.S]**
```hs
=> pure ($ y) <*> u
= pure ($ y) <*> Failure e
= Success ($ y) <*> Failure e                           {- from (i) -}
= Failure e                                             {- from (iii) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - `u = forall a b. Success (f :: (a -> b))`

**[L.H.S]**
```hs
=> u <*> pure y
= Success f <*> pure y
= Success f <*> Success y                               {- from (i) -}
= Success (f y)                                         {- from (ii) -}
```

**[R.H.S]**
```hs
=> pure ($ y) <*> u
= pure ($ y) <*> Success f
= Success ($ y) <*> Success f                           {- from (i) -}
= Success ($ y f)                                       {- from (ii) -}
= Success (f y)                                         {- since: f $ x = f x -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Interchange law is satisfied.

## Functor-Applicative relation
> fmap f x = pure f <\*> x; forall a b. (x :: forall e. Semigroup e => Validation e a), (f :: a -> b)

### Case a - `x = forall e. Semigroup e => Failure e`

**[L.H.S]**
```hs
=> fmap f x
= fmap f (Failure e)
= Failure e                                             {- from (vi) -}
```

**[R.H.S]**
```hs
=> pure f <*> x
= pure f <*> Failure e
= Success f <*> Failure e                               {- from (i) -}
= Failure e                                             {- from (iii) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - `x = forall a. Success a`

**[L.H.S]**
```hs
=> fmap f x
= fmap f (Success a)
= Success (f a)                                         {- from (vii) -}
```

**[R.H.S]**
```hs
=> pure f <*> x
= pure f <*> Success a
= Success f <*> Success a                               {- from (i) -}
= Success (f a)                                         {- from (ii) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Functor-Applicative relation is satisfied.
