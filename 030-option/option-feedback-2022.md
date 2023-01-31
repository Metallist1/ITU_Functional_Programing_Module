# Feedback on Hand-ins, Option, Fall 2022
Andrzej Wąsowski, 2022-09-26


## Reasons for failing

* There are still some, the script now generates minimal feedback, but
  reasons seem familiar:
  - 1X many tests failing (probably you chnaged imports, package name, or
    something like this)
  - 1X incorrect file format
  - 2X compilation failed? (please make sure it compiles and passes
    tests, do not be tempted to make even a slightest change between
    the last check and the submission; I have seen someone pulling in
    git, without merging the pull correctly)
  - 1X test process failing (you managed to crash the test framework)
  - several failing too many tests (I guess run out of time)
* Resubmissions make it hard to investigate, so I will probably stop
  doing that.
* Resubmission deadline, on Wednesdays, shortly before midnight.
  - we will try to keep it fixed, but cannot guarantee (some weeks we
    might not be ready with grades)
* Probably next week try to see how stats are developing (to see how
  many students are active). We seem to be about 99 active at the
  moment.


## Too long lines

This is actually important, hinders the examiners cognitive
abilities, can potentially lead to misunderstandings and irritation.

A __bad__ example:

```scala
  override def compare(that: java.awt.Point): Int =
    if (this.x < that.x || (this.x == that.x && this.y < that.y)) then -1 else if (this.x == that.x && this.y == that.y) then 0 else 1
```

Better:

```scala
  override def compare(that: java.awt.Point): Int =
    if this.x < that.x || (this.x == that.x && this.y < that.y) then -1
    else if this == that then 0
    else 1
```


A summary of differences:
- no parentheses around the if-branch condition
- break lines (a convention used in many programming languages is to
  start a new, unindented block at each else-if for chains of else-ifs)
- it seems that the second condition can be simplified (did not test)

Better? (not sure, patterns are weak, but reading is easier in  a
switch than in nested ifs)

```scala
  override def compare(that: java.awt.Point): Int = (this, that) match
    case _ if l.x < r.x || (l.x == r.x && l.y < r.y) => -1
    case _ if this == that => 0
    case _  => 1
```

## Too many braces

Mildly important, costs some decrease of trust in the solution

```scala
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
```

sightly better:

```scala
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
```

Similarly excess parentheses:

```scala
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf((f(value)))
    case Branch(l,r) => Branch((map(l)(f)),(map(r)(f)))
  }
```

Better:

```scala
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(value) => Leaf(f(value))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
```

Also

```scala
  def maximum1(t: Tree[Int]): Int = fold[Int,Int](t)((b1,b2)=>b1 max b2)((b)=>b)
```

better:

```scala
  def maximum1(t: Tree[Int]): Int =
    fold[Int, Int](t)((b1, b2) => b1 max b2)(b => b)
```

or even something like

```scala
  def maximum1(t: Tree[Int]): Int =
    fold[Int, Int](t)((b1, b2) => b1 max b2)(identity[Int])
```

Yield does not require parens either:


```scala
  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    for
      a <- ao
      b <- bo
    yield (f(a,b))
```

Slightly better:

```scala
  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] = for
    a <- ao
    b <- bo
  yield f(a,b)
```

Scala has a tendency to induce many parens, do not add your own, if
not needed ;)

## Tuple extraction

You can pattern match

```scala
val (_,r) = i
e(r)

i match
  case (_,r) => e(r)
```

but you can just use the selecting methods for tuples: `_._1` (and
`_._2`, and so on). so can use `i._2` instead of `r`. It is a matter
of taste what is more readable. I mix.

In a higher order function you can use eta-conversion:

```scala
map(list_of_pairs)(_._2)
```

instead of

```scala
map(list_of_pairs){ case (l,Nil) => l
                    case (l,tl ) => ... }
```
Again, this is a bit a metter of taste, but showing you the options.

## Power

Do not use `Math.pow` to square a number. Just use multiplication.

Math.pow (x,2) --> x*x

The former is a complex function that uses a call and a sophisticated
algorithm.  The latter is just a single CPU instruction.


All the issues mentioned above are things you want to fix __after you
have managed to pass the tests for an exercise__. Spent a few minutes
making the code nicer (refactoring).
