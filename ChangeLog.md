# Changelog for valida

## 1.0.0 (Aug 24, 2021)
* Add `profunctors` dependency.
* Add `Profunctor` instance for `Validator`.
* Rework `Validator` semigroup instance.

  The new semigroup instance now works similar to the `ValidationRule` semigroup instance.
* Add `Monoid` instance for `Validator`.
* Remove `ValidationRule`.
* Remove `Selector` type alias.
* Rewrite all previous `ValidationRule` combinators to work with `Validator`s instead.
* Rename `negateRule` -> `negateV`.
* Rename `falseRule` -> `failV`.
* Add `fixV` - refer to the documentation for more information.
* Remove `validate` - no longer necessary.
* Rename `labelV` -> `label`.
* Rename `<??>` -> `<?>`. Infix precedence 6.
* Remove `label` and `<?>`.
* `verify` is now an alias to flipped `lmap` from `Data.Profunctor`, specialized for `Validator`s.
* Allow usage with base == 4.12
* Explicitly constraint smallcheck dependency to 1.2.0+

## 0.1.0 (Aug 20, 2021)
Initial release.
