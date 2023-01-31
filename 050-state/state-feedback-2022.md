# Very quick notes after sampling solutions on State

## Stats

These are the stats for upto state:

```
         option lazy-list state
approved 93     93        73
no submi  7     11        16
act fail  3     4         19 !!!
```

Why so many failed? Did things work on your computers?

5 compilation failures, I was able to check 2, and both were obvious
submissions without running the test suite after the last change!

- one of them after fixing a white-space mishap, got 100% of points
  (apparently a Windows submission, so be careful with file format)

## General comments

I am still seeing too long lines (do not exceed 80 columns)

```
! state.Ex09.07: map2 gives same intDouble as the direct implementation: Falsified after 0 passed tests.
```

The test that fails:

```scala
  property("Ex09.07: map2 gives same intDouble as the direct implementation") =
    forAll { (rng: RNG) =>
      val intDouble =
        State(RNG.nonNegativeInt).map2 (State(RNG.double)) { (_, _) }
      intDouble.run(rng) == RNG.intDouble(rng)
    }
```

The test builds an implementation of intDouble using `map2` and gives
`nonNegativeInt` as the first argument, and `State(RNG.double)` as the
second argument.

So we need to sync the implementation of `map2` with the
implementation of `intDouble`.  This is the `map2` for this hand-in:

```scala
  // in class State
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap { a => sb.map(b => f(a, b)) }
```

and their implementation of `RNG.intDouble`:

```scala
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
```

Indeed the implementation of `intDouble` misses the condition that the
integers are supposed to be non-negative. Use `RNG.nonNegativeInt`
instead of `nextInt`.

_Thank you for helping with this bug_ - I have added the
non-negativity test to Exercise 3 (intDouble), so that students get
faster feedback next year.

__General lesson:__ please read the exercise text carefully and if
things still fail, please read the tests carefully.

## ints vs ints2

A similar but much more delicate comparison has to be done for ints vs
ints2 (which was a common problem.) One has to track how `sequence`
and `foldRight` interact against the direct implementation of `ints`.
