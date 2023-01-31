// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*
import adpro.state.RNG.SimpleRNG


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


// Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, rng2) = rng.nextInt
    (if i < 0 then -(i+1)
    else i, rng2)


  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (i, rng2) = nonNegativeInt(rng)
    val doublei = i.toDouble
    (if doublei > 1 then doublei / Int.MaxValue.toDouble
    else doublei, rng2)

  // Exercise 3

  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (randomInt, rng2) = nonNegativeInt(rng)
    val (randomDouble, rng3) = double(rng2)
    ((randomInt,randomDouble),rng3)


  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (randomInt, rng2) = nonNegativeInt(rng)
    val (randomDouble, rng3) = double(rng2)
    ((randomDouble,randomInt),rng3)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    if size > 0 then
      val (list, newRng) = ints(size-1)(rng)
      val (newInt, rng2) = newRng.nextInt
      (list.appended(newInt), rng2)
    else (Nil, rng)


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt) { i => i.toDouble / Int.MaxValue.toDouble }

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight[Rand[List[A]]](unit(Nil))((a,b) => map2(a,b) (_ :: _) )

  def ints2(size: Int): Rand[List[Int]] =
    sequence(List.fill(size)(int))

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt)( (i) => 
      val mod = i % bound
      if i + (bound-1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(bound)
    )

 
end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, s1) = run(s)
      val faState = f(a)
      faState.run(s1)
    }

  def map[B](f: A => B): State[S, B] = State { s => 
      val (a, state1) = run(s)
      (f(a), state1)          
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a,b),s2)
    }
    



object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]


  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
        sas.foldRight[State[S, List[A]]](unit(Nil))((a,b) => a.map2(b) (_ :: _) )

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    def nextStateValue(newState: S): LazyList[A] = 
      val (a,f) =s.run(newState)
      cons(a, nextStateValue(f))
    nextStateValue(initial)

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    val s = State[RNG, Int] { rng => rng.nextInt }
    stateToLazyList(s)(rng)

  lazy val tenStrictInts: List[Int] = 
    lazyInts(SimpleRNG(5)).take(10).toList

end State
