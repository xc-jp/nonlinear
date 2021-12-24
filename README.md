# nonlinear

Low-dimensional vectors.

More specifically, A clone of `linear`, but less general, and minimal dependencies.

### Differences from `linear`

Our primary focus is on providing a set of low-dimensional vector types (i.e. `V1` through `V4`), and having minimal dependencies.
We specifically don't aim to make functions general enough to be used for e.g. sparse vectors.

- Since `Representable` and `Distributive` can be very useful, they have been copied over from `adjunctions` and `distributive`.

- We don't use the `lens` library itself, but the provided lenses should be fully compatible with it and most other lens libraries

- `column` now only works on simple lenses (i.e. `Lens' s a`). This makes the implementation a lot simpler, and should cover normal use cases.

- We drop `Additive` and `Metric`, since for `V<n>` they can be fully captured by `Applicative` and `Foldable`

- Similarly, `Trace` has been dropped in favor of the `diagonal` and `trace` methods in `Nonlinear.Matrix` that use `Foldable` and `Monad`

- `Linear.Algebra` has been dropped because I don't know what it does
