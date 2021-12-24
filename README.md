# nonlinear

Low-dimensional vectors.

More specifically, A clone of `linear`, but less general, and minimal dependencies.

### Differences from `linear`

Our primary focus is on providing a set of low-dimensional vector types (i.e. `V1` through `V4`), and having minimal dependencies.
We specifically don't aim to make functions general enough to be used for e.g. sparse vectors.

- Lenses still work wonderfully, but we use `microlens` instead. (TODO: do we drop microlens as well?)

- For `Vn`, `Additive` is equal to `Applicative`, and so has been removed.

- `column` has been specialized to a `Lens'`
