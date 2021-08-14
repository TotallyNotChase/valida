# Proofs of Applicative Functor laws for `forall e inp a. Semigroup e => Validator e inp a`

```hs
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a }
```
where-
```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
pure :: a -> Validator e inp a
pure x = Validator (\inp -> Success x)                                                     {- (i) -}

(<*>) :: Semigroup e => Validator e inp (a -> b) -> Validator e inp a -> Validator e inp b
(Validator ff) <*> (Validator v) = Validator (\inp -> ff inp <*> v inp)                    {- (ii) -}

fmap :: (a -> b) -> Validator e inp a -> Validator e inp b
fmap f (Validator v) = Validator (\inp -> fmap f (v inp))                                  {- (iii) -}
```

```hs
pure :: a -> Validation e a
pure = Success                                                                             {- (iv) -}

(<*>) :: Semigroup e => Validation e (a -> b) -> Validation e a -> Validation e b
Success f <*> Success b = Success (f b)                                                    {- (v) -}
Success _ <*> Failure e = Failure e                                                        {- (vi) -}
Failure e <*> Success _ = Failure e                                                        {- (vii) -}
Failure x <*> Failure y = Failure (x <> y)                                                 {- (viii) -}

fmap :: (a -> b) -> Validation e a -> Validation e b
fmap f (Failure e) = Failure e                                                             {- (ix) -}
fmap f (Success a) = Success (f a)                                                         {- (x) -}
```

**Note**: The following proofs assume the `Validation` functor and applicative instances are lawful.

## Identity law
> pure id <\*> v = v; forall e inp a. Semigroup e => Validator e inp a

**[L.H.S]**
```hs
=> pure id <*> v
= pure id <*> Validator a
= Validator (\inp -> Success id) <*> Validator a                                             {- from (i) -}
= Validator (\inp' -> (\inp -> Success id) inp' <*> a inp')                                  {- from (ii) -}
= Validator (\inp' -> Success id <*> a inp')
= Validator (\inp' -> pure id <*> a inp')                                                    {- from (iv) -}
= Validator (\inp -> pure id <*> a inp)
= Validator (\inp -> a inp)                                                                  {- since: <*> identity law; from (iv) - (viii) -}
= Validator a
```

**[R.H.S]**
```hs
=> v
= Validator a
```

<ins>L.H.S = R.H.S</ins>

Thus, Identity law is satisfied.

## Composition law
> pure (.) <\*> u <\*> v <\*> w = u <\*> (v <\*> w); forall e inp a b c. (Semigroup e, Eq c) => (w :: Validator e inp a), (u :: Validator e inp (b -> c)), (v :: Validator e inp (a -> b))

**Note**: Denoting the three validators, `u`, `v`, and `w` as `Validator x`, `Validator y`, and `Validator z` respectively.

**[L.H.S]**
```hs
=> pure (.) <*> u <*> v <*> w
= pure (.) <*> Validator x <*> Validator y <*> Validator z
= Validator (\inp -> Success (.)) <*> Validator x <*> Validator y <*> Validator z            {- from (i) -}
= Validator (\inp' -> (\inp -> Success (.)) inp' <*> x inp') <*> Validator y <*> Validator z {- from (ii) -}
= Validator (\inp' -> Success (.) <*> x inp') <*> Validator y <*> Validator z
= Validator (\inp -> Success (.) <*> x inp) <*> Validator y <*> Validator z
= Validator (\inp' -> (inp -> Success (.) <*> x inp) inp' <*> y inp') <*> Validator z        {- from (ii) -}
= Validator (\inp' -> Success (.) <*> x inp' <*> y inp') <*> Validator z
= Validator (\inp -> Success (.) <*> x inp <*> y inp) <*> Validator z
= Validator (\inp' -> (\inp -> Success (.) <*> x inp <*> y inp) inp' <*> z inp')             {- from (ii) -}
= Validator (\inp' -> Success (.) <*> x inp' <*> y inp' <*> z inp')
= Validator (\inp -> Success (.) <*> x inp <*> y inp <*> z inp)
= Validator (\inp -> pure (.) <*> x inp <*> y inp <*> z inp)                                 {- from (iv) -}
= Validator (\inp -> x inp <*> (y inp <*> z inp))                                            {- since: <*> composition law; from (iv) - (viii) -}
```

**[R.H.S]**
```hs
=> Validator x <*> (Validator y <*> Validator z)
= Validator x <*> Validator (\inp -> y inp <*> z inp)                                        {- from (ii) -}
= Validator (\inp' -> x inp' <*> (\inp -> y inp <*> z inp) inp')                             {- from (ii) -}
= Validator (\inp' -> x inp' <*> (y inp' <*> z inp'))
= Validator (\inp -> x inp <*> (y inp <*> z inp))
```

<ins>L.H.S = R.H.S</ins>

Thus, Composition law is satisfied.

## Homomorphism law
> pure f <\*> pure x = pure (f x); forall a b. (x :: a), (f :: a -> b)

**[L.H.S]**
```hs
=> pure f <*> pure x
= Validator (\inp -> Success f) <*> Validator (\inp -> Success x)                            {- from (i) -}
= Validator (\inp' -> (\inp -> Success f) inp' <*> (\inp -> Success x) inp')                 {- from (ii) -}
= Validator (\inp' -> Success f <*> Success x)
= Validator (\inp -> Success f <*> Success x)
= Validator (\inp -> pure f <*> pure x)                                                      {- from (iv) -}
= Validator (\inp -> pure (f x))                                                             {- since: <*> homomorphism law; from (iv) - (viii) -}
```

**[R.H.S]**
```hs
=> pure (f x)
= Validator (\inp -> Success (f x))                                                          {- from (i) -}
= Validator (\inp -> pure (f x))                                                             {- from (iv) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Homomorphism law is satisfied.

## Interchange law
> u <\*> pure y = pure ($ y) <\*> u; forall a b. (y :: a), (u :: forall e inp. Semigroup e => Validator e inp (a -> b))

**[L.H.S]**
```hs
=> u <*> pure y
= Validator a <*> pure y
= Validator a <*> Validator (\inp -> Success y)                                              {- from (i) -}
= Validator (\inp' -> a inp' <*> (\inp -> Success y) inp')                                   {- from (ii) -}
= Validator (\inp' -> a inp' <*> Success y)
= Validator (\inp -> a inp <*> Success y)
= Validator (\inp -> a inp <*> pure y)                                                       {- from (iv) -}
```

**[R.H.S]**
```hs
=> pure ($ y) <*> u
= pure ($ y) <*> Validator a
= Validator (\inp -> Success ($ y)) <*> Validator a                                          {- from (i) -}
= Validator (\inp' -> (\inp -> Success ($ y)) inp' <*> a inp')                               {- from (i) -}
= Validator (\inp' -> Success ($ y) <*> a inp')
= Validator (\inp -> Success ($ y) <*> a inp)
= Validator (\inp -> pure ($ y) <*> a inp)                                                   {- from (iv) -}
= Validator (\inp -> a inp <*> pure y)                                                       {- since: <*> interchange law; from (iv) - (viii) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Interchange law is satisfied.

## Functor-Applicative relation
> fmap f x = pure f <\*> x; forall a b. (x :: forall e inp. Semigroup e => Validator e inp a), (f :: a -> b)

**[L.H.S]**
```hs
=> fmap f x
= fmap f (Validator a)
= Validator (\inp -> fmap f (a inp))                                                         {- from (iii) -}
```

**[R.H.S]**
```hs
=> pure f <*> x
= pure f <*> Validator a
= Validator (\inp -> Success f) <*> Validator a                                              {- from (i) -}
= Validator (\inp' -> (\inp -> Success f) inp' <*> a inp')                                   {- from (ii) -}
= Validator (\inp' -> Success f <*> a inp')
= Validator (\inp -> Success f <*> a inp)
= Validator (\inp -> pure f <*> a inp)                                                       {- from (iv) -}
= Validator (\inp -> fmap f (a inp))                                                         {- since: Functor applicative relation, from (iv) - (x) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Functor-Applicative relation is satisfied.
