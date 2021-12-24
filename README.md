# nonlinear

Low-dimensional vectors.

More specifically, a lightweight clone of the low-dimensional parts of [`linear`](https://hackage.haskell.org/package/linear).

### Differences from `linear`

Our primary focus is on providing a set of low-dimensional vector types (i.e. `V1` through `V4`), and avoiding dependencies.
We specifically don't aim to make functions/classes general/principled/robust enough to be used for e.g. sparse vectors.

- Since `Representable` and `Distributive` can be very useful, they have been copied over from `adjunctions` and `distributive`, respectively.

- We don't use the `lens` library itself, but the provided lenses should be fully compatible with it and most other lens libraries.

- `column` now only works on simple lenses (i.e. `Lens' s a`). This makes the implementation a lot simpler, and should cover normal use cases.

- We drop `Additive` and `Metric`, since for `V<n>` they can be fully captured by `Applicative` and `Foldable`.

- Similarly, `Trace` has been dropped in favor of the `diagonal` and `trace` methods in `Nonlinear.Matrix` that use `Foldable` and `Monad`

- `Linear.Epsilon` has been dropped since I consider robust close-to-zero checking out of scope for this library. `normalize = signorm`.

- `Linear.Algebra`, `Nonlinear.Plucker`, and `Linear.Covector` have been dropped for the time being because I don't see the point.

- Data types lack instances that would incur dependencies, e.g. `Random` and `Binary`.
