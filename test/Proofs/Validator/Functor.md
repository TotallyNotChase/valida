# Proofs of Functor laws for `forall e inp a. Validator e inp a`

```hs
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a }
```
where-
```hs
data Validation e a = Failure e | Success a
```

### <ins>Relevant equations</ins>:-
```hs
fmap :: (a -> b) -> Validator e inp a -> Validator e inp b
fmap f (Validator v) = Validator (\inp -> fmap f (v inp))  {- (i) -}
```

```hs
fmap :: (a -> b) -> Validation e a -> Validation e b
fmap f (Failure e) = Failure e                             {- (ii) -}
fmap f (Success a) = Success (f a)                         {- (iii) -}
```

**Note**: The following proofs assume the `Validation` functor instance is lawful.

## Identity law
> fmap id v = id v; forall v :: forall e inp a. Validator e inp a

**[L.H.S]**
```hs
=> fmap id v
= fmap id (Validator a)
= Validator (\inp -> fmap id (a inp))                         {- from (i) -}
= Validator (\inp -> id (a inp))                              {- since: fmap identity law; (ii) and (iii) -}
= Validator (\inp -> a inp)                                   {- since: id a = a -}
= Validator a
```

**[R.H.S]**
```hs
=> id (Validator a)
= Validator a                                                 {- since: id a = a -}
```

<ins>L.H.S = R.H.S</ins>

Thus, Identity law is satisfied.

## Composition law
> fmap (f . g) v = (fmap f . fmap g) v; forall a b c. Eq c => (v :: forall e inp. Validator e inp a), (f :: (b -> c)), (g :: (a -> b))

**[L.H.S]**
```hs
=> fmap (f . g) v
= fmap (f . g) (Validator a)
= Validator (\inp -> fmap (f . g) (a inp))                    {- from (i) -}
```

**[R.H.S]**
```hs
=> (fmap f . fmap g) v
= (fmap f . fmap g) (Validator a)
= (\x -> fmap f (fmap g x)) (Validator a)                     {- since: f . g = \x -> f (g x) -}
= fmap f (fmap g (Validator a))
= fmap f (Validator (\inp -> fmap g (a inp)))                 {- from (i) -}
= Validator (\inp' -> fmap f ((\inp -> fmap g (a inp)) inp')) {- from (i) -}
= Validator (\inp' -> fmap f (fmap g (a inp')))
= Validator (\inp' -> (fmap f . fmap g) (a inp'))             {- since: f . g = \x -> f (g x) -}
= Validator (\inp' -> fmap (f . g) (a inp'))                  {- since: fmap composition law; (ii) and (iii) -}
= Validator (\inp -> fmap (f . g) (a inp))
```

Thus, Composition law is satisfied.
