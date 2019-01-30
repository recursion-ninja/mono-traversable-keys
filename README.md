## Type-classes for interacting with monomorphic containers with a key

[![Build Status](https://travis-ci.org/recursion-ninja/mono-traversable-keys.svg?branch=master)](https://travis-ci.org/recursion-ninja/mono-traversable-keys)
[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mono-traversable-keys.svg?style=flat)](https://hackage.haskell.org/package/mono-traversable-keys)
[![Stackage Nightly](http://stackage.org/package/mono-traversable-keys/badge/nightly)](http://stackage.org/nightly/package/mono-traversable-keys)
[![Stackage LTS](http://stackage.org/package/mono-traversable-keys/badge/lts)](http://stackage.org/lts/package/mono-traversable-keys)

Provides type-classes for interacting with monomorphic containers in the following ways:

 * zipping
 * adjusting an element at a key
 * safe indexing with a key
 * unsafe indexing with a key
 * mapping with a key
 * folding with a key
 * traversing with a key
 * zipping with a key

This package is the extension of two other popular libraries, [`mono-traversable`](http://hackage.haskell.org/package/mono-traversable) and [`keys`](http://hackage.haskell.org/package/keys), designed to provided the functionality of the `keys` package to the monomorphic containers enhanced by the `mono-traversable` package.
