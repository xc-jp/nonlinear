# nonlinear

Low-dimensional vectors.

More specifically, a lightweight, opinionated clone of the low-dimensional vector parts of [`linear`](https://hackage.haskell.org/package/linear).

### Differences from `linear`

Our primary focus is on providing a set of low-dimensional vector types (i.e. `V1` through `V4`), and avoiding dependencies.
We specifically don't aim to make functions/classes general/principled/robust enough to be used for e.g. sparse vectors.
This means there is just a single type class, `Vec`, equivalent to `Traversable` `Representable`
Obviously, that makes certain operations more restrictive than they are in `linear`.

Anything related to projective/homogeneous coordinates has been moved to the `Nonlinear.Projective` namespace.
There is `Hom2` and `Hom3` for 2- and 3-dimensional homogeneous coordinates, respectively.
