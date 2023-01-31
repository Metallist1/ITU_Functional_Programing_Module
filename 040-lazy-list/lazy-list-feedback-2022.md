# Stats

2 compilation failure
3 with many tests failing
  (note: ask AW or TAs for feedback, no more
   feedback comes from learnIT than from running tests locally)
12 no submission

67 approved

ca. 72 students active in the lazy list week (A bit less than expexted?)
    (confused the learnIT says submitted 96, the spreadsheet
    downloaded from learnIT says 85 students. All very confusing)

PASSES:
4 persons failed 1 test
2 persons failed 2 tests
3 persons failed 3 tests
3 persons failed 4 tests
2 persons failed 5 tests

should give 53 persons passing all tests

# takeUnfold

```scala
def takeUnfold(n: Int): LazyList[A] =
  unfold(this, n) (acc => acc match
    case (Cons(h, t), n) if n > 1  => Some (h(), (t(), n-1))
    case (Cons(h, t), 1) => Some (h(), (empty, 0))
    case _ => None
    )
```

The second case can be simplified a bit, because it does the same job
as the first. And a bit of the last.

```scala
def takeUnfold(n: Int): LazyList[A] =
  unfold(this, n) (acc => acc match
    case (Cons(h, t), n) if n >= 1  => Some (h(), (t(), n-1))
    case (Cons(h, t), 0) => None
    case _ => None
    )
```

Now re-ordering the first two cases, allows to drop the "dreaded if"
(it is actually not dreaded, but marginally better without it; also
should be more efficient)

```scala
def takeUnfold(n: Int): LazyList[A] =
  unfold(this, n) (acc => acc match
    case (Cons(h, t), 0) => None
    case (Cons(h, t), n) => Some (h(), (t(), n-1))
    case _ => None
  )
```

Now there is  a thing called "partial functions" in scala which allows
to define a lambda directly by pattern matching (but one has to use
braces):

```scala
def takeUnfold(n: Int): LazyList[A] =
  unfold(this, n) {
    case (Cons(h, t), 0) => None
    case (Cons(h, t), n) => Some (h(), (t(), n-1))
    case _ => None
  }
```

All these small improvements give us slightly better code, but only
slightly.

The original code fails the test of equivalence with
`take` (without unfold). The final does not. What has happened? Let's
look into the test:

```scala
property("Ex13.02: takeUnfold equivalent to take") =
  forAll { (l: LazyList[Int], n: Int) =>
    l.take(n).toList == l.takeUnfold(n).toList }
```

And take from the same work:

```scala
def take(n: Int): LazyList[A] = this match
  case Empty => Empty
  case Cons(h, t) =>
    if n != 0 then Cons(h, () => t().take(n-1)) else Empty
```

For these kind of tests (there are more every week), you have too look
at the two functions and see whether they are sync. In here it seems
that for negative `n` the original `take` would return the entire lazy
list, while the original `takeUnfold` would return Empty.

The exercise was not specific what to do for negative `n` (`take` does
not have to behave well for negative `n` but for such a test to pass
you need to sync the two implementations.  By refactoring, I have
inadvertendly ignored behavior on negative `n` and made it behave as
the author's original `take`)
