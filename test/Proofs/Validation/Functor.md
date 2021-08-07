# Proofs of Functor laws for `forall e a. Validation e a`

```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
fmap :: (a -> b) -> Validation e a -> Validation e b
fmap f (Failure e) = Failure e                       {- (i) -}
fmap f (Success a) = Success (f a)                   {- (ii) -}
```

## Identity law
> fmap id v = id v; forall v :: forall e a. Validation e a

### Case a - `v = forall e. Failure e`

**[L.H.S]**
```hs
=> fmap id v
= fmap id (Failure e)
= Failure e                             {- from (i) -}
```

**[R.H.S]**
```hs
=> id v
= id (Failure e)
= Failure e                             {- since: id a = a -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - `v = forall a. Success a`

**[L.H.S]**
```hs
=> fmap id v
= fmap id (Success a)
= Success (id a)                        {- from (i) -}
= Success a                             {- since: id a = a -}
```

**[R.H.S]**
```hs
=> id v
= id (Success a)
= Success a                             {- since: id a = a -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Identity law is satisfied.

## Composition law
> fmap (f . g) v = (fmap f . fmap g) v; forall a b c. Eq c => (v :: forall e. Validation e a), (f :: (b -> c)), (g :: (a -> b))

### Case a - `v = forall e. Failure e`

**[L.H.S]**
```hs
=> fmap (f . g) v
= fmap (f . g) (Failure e)
= Failure e                             {- from (i) -}
```

**[R.H.S]**
```hs
=> (fmap f . fmap g) v
= (fmap f . fmap g) (Failure e)
= (\x -> fmap f (fmap g x)) (Failure e) {- since: f . g = \x -> f (g x) -}
= fmap f (fmap g (Failure e))
= fmap f (Failure e)                    {- from (i) -}
= Failure e                             {- from (i) -}
```

<ins>L.H.S = R.H.S</ins>

### Case b - `v = forall a. Success a`

**[L.H.S]**
```hs
=> fmap (f . g) v
= fmap (f . g) (Success a)
= Success ((f . g) a)                   {- from (i) -}
= Success ((\x -> f (g x)) a)           {- since: f . g = \x -> f (g x) -}
= Success (f (g a))
```

**[R.H.S]**
```hs
=> (fmap f . fmap g) v
= (fmap f . fmap g) (Success a)
= (\x -> fmap f (fmap g x)) (Success a) {- since: f . g = \x -> f (g x) -}
= fmap f (fmap g (Success a))
= fmap f (Success (g a))                {- from (i) -}
= Success (f (g a))                     {- from (i) -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Composition law is satisfied.
