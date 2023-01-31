// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using options "-deprecation", "-feature", "-source:future", "-language:adhocExtensions"

package adpro.monad

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Prop.given
import org.scalactic.Equality
import org.scalactic.TripleEquals.*

/***** Part I. Monoid */

trait Monoid[A]:

  def combine(a1: A, a2: A): A
  def empty: A
  
  // Some Laws for monoids (We place them here like in Parsers)

  object laws:

    /* The instance of Eqaulity type class will be used for equality
     * comparisons.  Normally the default equality is fine, but we need to
     * parameterize to handle endoMonoid (comparisons of functions).  See more
     * comments about that in the test of Ex03.01. */

    def associative(using Arbitrary[A], Equality[A]): Prop =
      forAll { (a1: A, a2: A, a3: A) =>
        combine(combine(a1, a2), a3) === combine(a1, combine(a2, a3)) 
      } :| "monoid-associative"

    /* The triple equality below (===) uses the eqA instance (by default the
     * same as == but we can override it, which is exploited in the test of
     * Exercise 3) */

    def unit(using Arbitrary[A], Equality[A]): Prop = 
      forAll { (a: A) =>
        (combine(a, empty) === a) :| "right-empty" && 
        (combine(empty, a) === a) :| "left-empty" 
      } :| "monoid-unit"

    def monoid (using Arbitrary[A], Equality[A]): Prop =
      (associative && unit) :| "monoid-laws"

end Monoid

val stringMonoid = new Monoid[String]:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""

def listMonoid[A] = new Monoid[List[A]]:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil


// Exercise 1

lazy val intAddition: Monoid[Int] = new:
  def combine(x: Int, y: Int) = x + y
  val empty = 0

lazy val intMultiplication: Monoid[Int] = new:
  def combine(x: Int, y: Int) = x * y
  val empty = 1

lazy val booleanOr: Monoid[Boolean] = new:
  def combine(x: Boolean, y: Boolean) = x || y
  val empty = false

lazy val booleanAnd: Monoid[Boolean] = new:
  def combine(x: Boolean, y: Boolean) = x && y
  val empty = true


// Exercise 2

// a)

def optionMonoid[A]: Monoid[Option[A]] = new:
  def combine(x: Option[A], y: Option[A]) = x orElse y
  val empty = None


// b)

def optionMonoidLift[A: Monoid]: Monoid[Option[A]] = new:
  def combine(x: Option[A], y: Option[A]) = x orElse y
  val empty = None


// Exercise 3

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(f: A => A, g: A => A): A => A = f andThen g
  val empty: A => A = identity


// Exercise 4 (tests exercises 1-2, written by student)

object MonoidEx4Spec 
  extends org.scalacheck.Properties("exerc4"):

  property("Ex04.01: intAddition is a monoid") =
    intAddition.laws.monoid

  property("Ex04.02: intMultiplication is a monoid") =
    intMultiplication.laws.monoid

  property("Ex04.03: booleanOr is a monoid") =
    booleanOr.laws.monoid

  property("Ex04.04: booleanAnd is a monoid") =
    booleanAnd.laws.monoid

  property("Ex04.05: optionMonoid is a monoid") =
    optionMonoid[Char].laws.monoid

  property("Ex04.06: optionMonoidLift is a monoid") = 
    optionMonoidLift(using intAddition).laws.monoid

end MonoidEx4Spec


// Exercise 5

// We implement this as an extension, so that the exercises do not jump back
// and forth in the file. (This naturally belongs to the Monoid trait).

extension [B](mb: Monoid[B])
  def foldMap[A](as: List[A])(f: A => B): B =
    as.foldLeft(mb.empty)((b, a) => mb.combine(b, f(a)))


// Exercise 6

/* We implement this as an extension, so that the exercises do not jump back
 * and forth in the file. (This naturally belongs to the Monoid trait).
 *
 * The triple equality (===) uses the Equality instance to check equality
 * (by default the same as == but we can override it). */

extension [A: Arbitrary, Equality](ma: Monoid[A])

  def homomorphism[B](f: A => B)(mb: Monoid[B]): Prop =
      forAll { (x: A, y: A) =>
         mb.combine(f(x), f(y)) === f(ma.combine(x, y))
      }

  def isomorphism[B: Arbitrary](f: A => B, g: B => A)(mb: Monoid[B]): Prop =
      forAll { (x: B, y: B) =>
        homomorphism(f)(mb)  &&
        ma.combine(g(x), g(y)) === g(mb.combine(x, y))
      }


// Exercise 7 (tests for Exercise 6, written by the student)

object MonoidEx7Spec 
  extends org.scalacheck.Properties("exerc7"):

  property("Ex07.01: stringMonoid is isomorphic to listMonoid[Char]") =
    stringMonoid.isomorphism( x => x.toList, x => x.mkString)(listMonoid[Char])


// "Exercise 8 (tests for Exercise 1, written the by student)

object MonoidEx8Spec 
  extends org.scalacheck.Properties("exerc8"):

  property("Ex08.01: booleanOr is isomorphic to booleanAnd") =
    booleanOr.isomorphism( x => !x, x => !x)(booleanAnd)


// Exercise 9
def productMonoid[A, B](ma: Monoid[A])(mb: Monoid[B]): Monoid[(A, B)] = new:
  def combine(a1: (A, B), a2: (A, B)): (A, B) = (ma.combine(a1._1, a2._1) , mb.combine(a1._2, a2._2))
  val empty = (ma.empty,mb.empty)


// Exercise 10 (tests for Exercise 9, written by the student)

object MonoidEx10Spec 
  extends org.scalacheck.Properties("exer10"):

  property("Ex10.01: productMonoid(optionMonoid[Int])(listMonoid[String]) gives a monoid") =
    productMonoid(optionMonoid[Int])(listMonoid[String]).laws.monoid


/* This will be used in the Foldable below: We can get the dual of any monoid
 * just by flipping the `combine`. */

def dual[A](m: Monoid[A]): Monoid[A] = new:
  def combine(x: A, y: A): A = m.combine(y, x)
  val empty = m.empty

/***** Part II. Foldable */

/* The functions foldRight and foldLeft below are little gems for self-study :). 
 * They resemble the foldLeft via foldRight exercise from the begining of
 * the course. */

trait Foldable[F[_]]:

  extension [A](as: F[A])

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B

    def foldRight[B](z: B)(f: (A, B) => B): B =
      as.foldMap[B => B](f.curried)(using endoMonoid[B])(z)

    def foldLeft[B](z: B)(f: (B, A) => B): B =
      as.foldMap[B => B](a => b => f(b, a))(using dual(endoMonoid[B]))(z)

    def combineAll(using m: Monoid[A]): A =
      as.foldLeft(m.empty)(m.combine)

end Foldable

// Exercise 11

given foldableList[A]: Foldable[List] = new Foldable[List]:
  extension [A](as: List[A])
    override def foldMap[B](f: A => B)(using mb: Monoid[B]) = as.foldRight(mb.empty)((a, b) => mb.combine(f(a), b))

 
// Exercise 12

// Note since Foldable[F] is a given, its extensions for as are visible
// (https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html)

extension [F[_]: Foldable, A] (as: F[A])
  def toList: List[A] = as.toListF
  def toListF: List[A] = as.foldRight(List.empty[A])(_ :: _)



/***** Part III. Functor */

trait Functor[F[_]]:

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) 
    def distribute: (F[A], F[B])= 
      (fab.map(_._1), fab.map(_._2))

  extension [A, B](e: Either[F[A], F[B]]) 
    def codistribute: F[Either[A, B]] = e match
      case Left(fa) => fa.map { Left(_) }
      case Right(fb) => fb.map { Right(_) }

  object functorLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def map[A](using Arbitrary[F[A]], Equality[F[A]]): Prop =
      forAll { (fa: F[A]) => fa.map[A](identity[A]) === fa }

  end functorLaws

end Functor


// Exercise 13

lazy val optionFunctor: Functor[Option] = new:
  extension [A] (as: Option[A])
    def map[B](f: A => B): Option[B] = as.map(f)


// this instance is provided
val listFunctor: Functor[List] = new:
  extension [A] (as: List[A])
    def map[B](f: A => B): List[B] = as.map(f)

object FunctorEx14Spec 
  extends org.scalacheck.Properties("exer14"):

  property("Ex14.01: listFunctor satisfies the map law") =
    listFunctor.functorLaws.map[Int]

  // Exercise 14

  property("Ex14.02: optionFunctor satisfies map law (tests Exercise 13)") =
    optionFunctor.functorLaws.map[Int]

end FunctorEx14Spec


/***** Part IV. Monad */

trait Monad[F[_]] 
  extends Functor[F]: 

  def unit[A](a: => A): F[A]
  
  extension [A](fa: F[A])
    def flatMap[B] (f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      fa.flatMap[B] { a => unit(f(a)) }

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap { a => fb.map { b => f(a,b) } }
    
  object monadLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */
 
    def associative[A, B, C](using Arbitrary[F[A]], Arbitrary[A => F[B]], 
      Arbitrary[B => F[C]], Equality[F[C]]): Prop = 
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = x.flatMap[B](f).flatMap[C](g)
        val right = x.flatMap[C] { a => f(a).flatMap[C](g) }
        (left === right) :| s"left:$left right:$right"
      }
 
    def identityRight[A](using Arbitrary[F[A]], Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (x: F[A], f: A => F[A]) => 
        val result = x.flatMap[A](unit[A])
        (result === x) :| s"got:$result expected:$x" }
 
    def identityLeft[A: Arbitrary](using Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (y: A, f: A => F[A]) =>
        val left = unit[A](y).flatMap[A](f) 
        val right = f(y) 
        (left === right) :| s"left:$left right:$right"
      }
 
    def identity[A: Arbitrary](using Arbitrary[F[A]], Arbitrary[A => F[A]], 
      Equality[F[A]]): Prop =
      { "identity left: " |: identityLeft[A]  } &&
      { "identity right:" |: identityRight[A] }
 
    def monad[A: Arbitrary, B, C] (using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Arbitrary[A => F[B]], Arbitrary[B => F[C]]): Prop = 
      { "associative:" |: this.associative[A,B,C] } &&
      { "identity:   " |: this.identity[A] }

  end monadLaws
 
end Monad


// Exercise 15

lazy val optionMonad: Monad[Option] =  new:
  def unit[A](a: => A) = Some(a)
  extension [A](fa: Option[A])
    def flatMap[B](f: A => Option[B]) =  fa.flatMap(f)

lazy val listMonad: Monad[List] = new:
  def unit[A](a: => A) = List(a)
  extension [A](fa: List[A])
    def flatMap[B](f: A => List[B]) = fa.flatMap(f)

// Exercise 16 (tests for Exercise 15, written by the student)

object FunctorEx16Spec 
  extends org.scalacheck.Properties("exer16"):

  property("Ex16.01: optionMonad is a monad") =
    optionMonad.monadLaws.monad[Int,Int,Int]

  property("Ex16.02: listMonad is a monad") =
    listMonad.monadLaws.monad[Int,Int,Int]

end FunctorEx16Spec


// Exercise 17
//
// We do this as an extension to maintain the linear sequence of exercises in
// the file (we would normally place it in the Monad trait).

extension [F[_]](m: Monad[F]) 
  def sequence[A](fas: List[F[A]]): F[List[A]] = 
    fas.foldRight(m.unit(List[A]()))((fa, acc) => m.map2(fa)(acc)(_ :: _))

// Exercise 18

extension [F[_]](m: Monad[F]) 
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if n <= 0 then m.unit(Nil)
    else  m.map2(ma)(replicateM(n - 1, ma))(_ :: _)

// Write in the answer below ...
//
// Since its a recursive function. ReplicateM checks if N is not 0 and if its not. 
// It takes any value given (as F is generic) and generates a list using this monad.
// The last value is unit(Nil) and each head is the same object ma that is being passed along.
// Types dont matter, as its a generic function.


// Exercise 19

extension [F[_]](m: Monad[F]) 
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => m.flatMap(f(a))(g)
