## 0.3.0 (3/6/2019)

* Switch backend of the library to build on the hedgehog library
* Change `Test` from kind `* -> *` to kind `*`. See [my note](https://github.com/joelburget/easytest/issues/22#issuecomment-469039853) for motivation. In short, this fixes a lot of bugs and we now support property testing.

Upgrading:

`Test` now has kind `*`. It's no longer a functor, applicative, monad, etc.
- You can build an atomic test with `unitTest` (/ `example`) or `propertyTest`.
- Also see `bracket`, `bracket_`, and `finally` for tests with setup / teardown.
- `tests` and `scope` work as before. I mention them here because they're the
  other way to build tests.

Removed:
- `expect b`          -> `assert b`
- `expectEq a b`      -> `a === b`
- `expectJust`        -> `matches _Just`
- `expectRight`       -> `matches _Right`
- `expectRightNoShow` -> `matches _Right`
- `expectLeft`        -> `matches _Left`
- `expectLeftNoShow`  -> `matches _Left`

## 0.2.1 (10/24/2018)

* [Fix build errors for GHC 8.6](https://github.com/joelburget/easytest/commit/9bb30ec16671c0ec74835a52290b6508143a368f), [prevent building on GHC before 7.10](https://github.com/joelburget/easytest/pull/15/commits/f6d0ac50fa5a351a30b576567306121d67c0973a)
* [Only print emojis for Unicode-capable terminals](https://github.com/joelburget/easytest/commit/e3f12612df46a6367693fd4ad47eedf91c35a079)

## 0.2 (3/27/2018)

* [`expectRight` now shows `Left`s. `expectRightNoShow` replicates the old functionality.](https://github.com/joelburget/easytest/commit/c2d5dccc97dcdb925ebc39c36fcde9ff8d894f77)
* [Call stacks now longer show EasyTest porcelain.](https://github.com/joelburget/easytest/commit/0b7064915a5b9c9de0115ebb6fc2fa49b2c4776e)
* [`expectJust` and `expectRight` now return unit](https://github.com/joelburget/easytest/commit/ef5d4e9fd03c1008c810ee09a4f4c459d4e26bdb)

## 0.1.1 (3/25/2018)

* [Add ghc 7.10.3 compatibility.](https://github.com/joelburget/easytest/commit/4acfad507cefc3fb2c0d588f1fbe0e4d583a762d)
* [allow async 2.2, ghc 8.4 compatibility](https://github.com/joelburget/easytest/commit/6c20c18988dd756d8088c9d0318b597be15c9229)
* [build with latest stackage nightly](https://github.com/joelburget/easytest/commit/2ea7f7520b39ac74b576414e4e1df75f596ed7b4)

## 0.1 (3/6/2018)

Initial release.
