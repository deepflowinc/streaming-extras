# streaming-extras

Missing extra libraries for the [`streaming`][streaming] and [`streaming-bytestring`][streaming-bytestring].

[streaming]: https://hackage.haskell.org/package/streaming
[streaming-bytestring]: https://hackage.haskell.org/package/streaming-bytestring

## Contribution

Please feel free to open an issue, but also please search for existing issues to check if there already is a similar one.

See [CONTRIBUTING.md][CONTRIBUTING] for more details.

[CONTRIBUTING]: ./CONTRIBUTING.md

## Matrix Build

To add/remove a specific ghc version to/from the tested versions, you can just add/remove cabal's freeze file as `ci/configs/ghc-X.X.X.project` and add the following to the header:

```cabal
import: ../../cabal.project
-- FIXME: Use Appropriate timestamp for index-state
index-state: hackage.haskell.org 2024-02-05T22:44:18Z
```

Note that the actual value of `index-state` should be sufficiently new.

Except this, no modification to Action worklow file is needed in general.
The following is the example command to add the `ghc-9.8.1` with the most recent Stackage Nightly:

```bash
curl --location https://www.stackage.org/nightly/cabal.config > ./ci/configs/ghc-9.8.1.project
cat <<EOF >>./ci/configs/ghc-9.8.1.project
import: ../../cabal.project
-- FIXME: Use an appropriate timestamp for index-state
index-state: hackage.haskell.org 2024-02-05T22:44:18Z
EOF
```

Note that we might have to edit `with-compiler` stanza of the downloaded freeze file when you want to test GHC version different from Stackage's default version.

If you want to test some breeding-edge version of GHC but to allow failure, name freeze file as `ghc-X.X.X-head.project`.

```bash
curl --location https://www.stackage.org/nightly/cabal.config > ./ci/configs/ghc-9.10.1-head.project
```

## Copyright

2024 (c) DeepFlow, Inc.
