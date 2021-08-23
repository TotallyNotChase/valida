# Changelog for valida

## Unreleased changes
* Removed `ValidationRule`.
* Added `Profunctor` instance for `Validator`.
* Reworked `Validator` semigroup instance.

  The new semigroup instance now works similar to the `ValidationRule` semigroup instance.
* Added `Monoid` instance for `Validator`.
* Removed `Selector` type alias.
* Rewritten all previous `ValidationRule` combinators to work with `Validator`s with output type `()` instead.
* Renamed `negateRule` -> `negateV`
* Renamed `falseRule` -> `failV`.
* Added `fixV` - refer to the documentation for more information.
* Removed `validate`. Use `fixV` instead.
* Removed `label` and `<?>`.
* Renamed `labelV` -> `label`.
* Renamed `<??>` -> `<?>`. Infix precedence 6.
* `verify` is now an alias to flipped `lmap` from `Data.Profunctor`, specialized for `Validator`s.

## 0.1.0 (Aug 20, 2021)
Initial release.
