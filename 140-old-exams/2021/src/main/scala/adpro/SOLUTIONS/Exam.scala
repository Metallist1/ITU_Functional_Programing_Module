package adpro
package SOLUTIONS

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._
import org.scalactic.Equality

object Q1 {

  /*** Warm-up
   *
   * Consider the following example of a simple Java class hierarchy. The
   * example is written using Scala syntax so that we do not have to mix
   * languages in the exam.  Recall that in Java all method calls are virtual,
   * so dynamically dispatched.
   *
   * class Printable { def hello (): String = "printable" }
   * class Triangle extends Printable { override def hello (): String = "triangle" }
   * class Square extends Printable { override def hello (): String = "square" }
   *
   * Now the following is an ADT in Scala, that realizes the same hierarchy:
   */

  trait Printable
  case object Triangle extends Printable
  case object Square extends Printable

  /* Q1. (5 %)
   *
   * Write a function `hello` that uses pattern matching and achieves exactly
   * the same side effect as calling the method hello in Java implementation.
   * Uncomment the definition and fill in the gaps.
   */

  def hello (p: Printable): String = p match {
    case Triangle => "triangle"
    case Square => "square"
    case _ => "printable"
  }

  // Two attempts to understand how a WRONG answer could look like

  // def hello__ (p: Printable): Unit = p match {
  //   case p: Printable => print ("printable")
  //   case p: Triangle => print ("triangle")
  //   case p: Square => print ("square")
  // }

  // The following is less broken wrong, but slightly conceptually suboptimal
  // (Triangle and Square are apparently no longer types in Scala, I think they
  // used to be).

  // def hello___ (p: Printable): Unit = p match {
  //   case p: Triangle => print ("triangle")
  //   case p: Square => print ("square")
  //   case _ => print ("printable")
  // }
}



object Q2 {

  /*** Sequence and Either
   *
   * Q2. (10%)
   *
   * Implement the `sequence` function for Either. The behavior should be like
   * with `sequence` for option: A single failure (left) on a list should result
   * in a failure overall.  For the error value in this case return the last
   * error value seen on the list.
   */

  def sequence [Err,A] (as: List[Either[Err,A]]): Either[Err, List[A]] =
    as.foldRight[Either[Err,List[A]]] (Right (Nil)) {
      (ea, eas) =>
        for {
          as <- eas
          a <- ea
        } yield a::as
    }

  // A solution without for-yield:

  def f[Err,A] (ea: Either[Err,A], eas: Either[Err, List[A]]): Either[Err, List[A]] =
    eas.flatMap { l => ea.map { a => a::l } }
  def sequence__[Err,A] (as: List[Either[Err,A]]): Either[Err, List[A]] =
    as.foldRight[Either[Err,List[A]]] (Right (Nil)) (f)

  // Some thinking on possible mistakes:
  //
  // - I think using foldLeft is likely returning the first, not the last error.
  // - Also direct recursion with a tail-recursive accumulator trick is likely
  //   giving the first error.
  // - A translation to Option is lossing the information on failures.
  //
  // There is this solution easy to find online:
  // https://stackoverflow.com/questions/7230999/how-to-reduce-seqeithera-b-to-eithera-seqb
  // (two TAs found it). However it uses different types List vs Seq, Err vs
  // String. Also it uses the method _.right  which was not implemented in our
  // course and it is not in std lib.  (It is a cats method):
  // def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
  //
  // So I think this is still a risky exercise, worth keeping.  It can be used
  // to reward actual knowledge from last moment searching. This is the
  // stackover solution
  //
  // def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
  //   s.foldRight(Right(Nil): Either[A, List[B]]) {
  //     (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
  //   }
  //
  // The use of Seq and B, along with .right are characteristic.
  //
  // Also it seems that acc.right and e.right is actually a mistake by the
  // poster. This will compile but .right seems unnecessary.  It just packs and
  // umpacks the Right value into RightOps needlessly.  (My solution, above,
  // does well without it).
}



object Q3 {

  /*** Typing
   *
   * Imagine that we want to implement `sequence` with Either, but using any
   * arbitrary collection F[_] for which we know that it is Foldable, instead of
   * List.
   *
   * Q3. (10%)
   *
   * Write the type signature for this new function 'sequence', enforcing a
   * suitable type constraint on F.
   *
   * Do not implement the function, just put '???' in the body.
   */

   def sequence[Err,A, F[_]: Foldable] (as: F[Either[Err,A]]): Either[Err, F[A]] = ???
}



object Q4 {

  /*** Random Value Generators
   *
   * Recall that the type State.Rand[A] is defined as follows:
   *
   * type Rand[A] = State[RNG, A]
   *
   * Q4. (10%)
   *
   * Implement a *pure* generator of triples, where the first two components are
   * random integers 'a' and 'b', whereas the third component 'x' is a random
   * double number between them, so the following constraint is satisfied:
   *
   *     a <= x <= b
   */

  import State._

  val riid : Rand[(Int,Int,Double)] =
    for {
      a <- State (RNG.int)
      b <- State (RNG.int)
      l = Math.min (a, b)
      r = Math.max (a ,b)
      x <- State (RNG.double)
    } yield (l, r, l + (r-l).toDouble.abs * x)

  // A worse solution could use terribly nested maps, flatMaps (still fine, if
  // they are written as separate let-expressions or laidout readably).
  //
  // Also a (slightly) worse solution would construct `riid` from scratch using
  // RNG.nextInt, instead of composing existing generators declaratively.
  //
  // A big problem is to use imperative API, and actually create numbers
  //
  // A weak partial solution would just generate two ints and a double, without
  // ensuring any constraints between them.
}



object Q5 {

  /*** Type Extensions
   *
   * Recall that we have two representations of Rand in State.scala:
   *
   * - a type called RNG.Rand:
   *
   *   type Rand[+A] = RNG => (A, RNG)
   *
   * - a type called State.Rand:
   *
   *   type Rand[A] = State[RNG, A]
   *
   * In State.scala many more functions are available for RNG.Rand than for
   * State.Rand (because State is a more abstract interface that can be used for
   * other things, so it does not know that we are dealing with RNG inside). One
   * example of such function is `Rand.both`
   *
   * def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)]
   *
   * Q5. (5%)
   *
   * Make all functions of RNG.Rand available for State.Rand by a suitable
   * type conversion (the question continues below):
   */

  // ...

  // This solution allows to convert any State.Rand[A] value to an RNG.Rand[A]
  // value implicitly, and then all methods of RNG.Rand[A] methods can be
  // called.

  implicit def State2RNG[A] (st: State.Rand[A]): RNG.Rand[A] =
    st.run

  // This is a slightly less clear answer but it also makes sense (it allows to
  // use an RNG value wherever a State is needed, which has a similar effect:
  // you can work with RNG values as long as you want, using their methods, and
  // promote them to State late - this would have the right effect in the riid
  // implementation above, so I would grade it full points.
  implicit def RNGisState_ [A] (ra: RNG.Rand[A]): State.Rand[A] =
    State (ra)

  // A perfect answer (not really expected, should probably achieve both
  // conversions). This is probably hard to get right in practice, because
  // cyclic implicit conversions will break the compiler. I do not expect it
  // then.

  // A longer, but more puristic, answer is to implement a set of extension
  // methods for state that are forwarded to RNG.Rand.  This is more work but
  // has other advantages so should get full points (no issues with cyclicity of
  // implicits).
  //
  // Delegating just the both function is a passable solution, but weak.
}

/* Q6. (5%)
 *
 * Explain in English the mechanism you have used to achieve this. How does
 * your solution achieve the objective of Q5?
 */

/* write here ... (see above for notes that amount to an answer)
 */


object Q7 {

  /*** Lazy Streams
   *
   * Let's assume that we have the following function 'size' implemented that
   * computes the lengths of a lazy stream. (The function should be correct, no
   * point to seek traps in it.)
   */

  def size[A] (s: Stream[A]): Int = {
    def f (s: Stream[A], acc: Int): Int = s match {
      case Cons (_, t) =>  f (t (), acc+1)
      case Empty => acc
    }
    f (s, 0)
  }

  /* Q7. (5%)
   *
   * What is the problem with writing s.size >= 10 to check whether the stream
   * is at least 10 elements? Explain.
   */

  // Write here ...

  // The Stream.size function iterates the entire tail of the Stream until it
  // hits the empty node - this may not terminate, use a lot of time, and/or a
  // lot of memory.  Technically, this is not needed, because after iterating 10
  // elements we can decide the value of the predicate.

}



object Q8 {

  import Q7.size

  /**** A Better Size for Streams
   *
   * Q8. (5%)
   *
   * Implement a pure function `checkIfLongerEqThan` that checks whether a stream
   * is longer than a given bound. Do not use the standard library functions
   * lengthCompare, sizeCompare, lenghtIs, or sizeIs. You can use 'size' from
   * above, or any other functions from the course.
   */

  def  checkIfLongerEqThan [A] (s: Stream[A]) (n: Int): Boolean =
    size (s.take (n)) == n

  def  checkIfLongerEqThan_ [A] (s: Stream[A]) (n: Int): Boolean =
    (n == 0) || s.drop (n-1) != Empty

  // This solution is impure (yuck) and incorrect, because drop does not throw
  // an exception just returns empty
  def  checkIfLongerEqThan__ [A] (s: Stream[A]) (n: Int): Boolean =
    try { s.drop (n); true } catch { case e: Throwable => false }

  // A direct recursive solution is also possible, I would like it slightly
  // less. We have directly discouraged such.
}



object Q9 {

  import SOLUTIONS.Q8.checkIfLongerEqThan

  /*** Testing
   *
   * Assume we have a solution for question Q8 (even if you skipped it), so that
   * we have a function:
   *
   *    checkIfLongerEqThan[A] (s: Stream[A]) (n: Int): Boolean
   *
   * that returns true if and only if the stream 's' has at least 'n' elements.
   *
   *
   * Q9. (10%)
   *
   * Use it to write a property-based test that checks if every nonEmpty stream
   * concatenated with itself is longer than 2.
   */

  class MySpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

    import org.scalacheck.Gen

    implicit lazy val arbStream: Arbitrary[Stream[Int]] =
      Arbitrary {
        Gen.listOf (Gen.choose (1, 100))
           .map { l => Stream (l: _*) }
      }

    // an example solution
    "A nonEmpty stream concatenated with itself is at least 2 elements long" in {
      forAll { s: Stream[Int] =>
        whenever (s != Empty) {
          checkIfLongerEqThan (s append s) (2) should be (true)
        }
      }
    }

    // Some notes on possible bad solutions:
    //
    // - Testing on a single stream is bad.  One should use checkIfLongerEqThan
    //   forAll, and the arbStream.
    // - Invoking the original length on streams or toList, would be against the
    //   course's intention.  It is not clear if arbStream only generates finite
    //   streams.

  }
}



object Q10 {

  import Par._

  /*** Your Parallel Options
   *
   * Q10. (10%)
   *
   * Write a function which 'flattens' a Option[Par[A]] value to a Par[Option[A]]
   * value, for any type 'A'.
   */

  def flatten [A] (opa: Option[Par[A]]): Par[Option[A]] =
    opa match {
      case None => Par.unit[Option[A]] (None)
      case Some (pa) => Par.map[A, Option[A]] (pa)  (a => Some (a))
    }

  // This is a complex solution that derives from the list solution in the
  // files.  In this case I find the pattern matching solution simpler.
  def flatten__ [A] (opa: Option[Par[A]]): Par[Option[A]] =
    opa.foldRight[Par[Option[A]]] (unit(None)) { (pa,poa) =>
      Par.map2 (pa,poa) ((a,oa) => Some (a) ) }

  // This one relies on noticing that this is sequence and reducing to the list
  // problem:
  def flatten___ [A] (opa: Option[Par[A]]): Par[Option[A]] =
    Par.map (Par.sequence (opa.toList)) { _ match {
      case Nil => None
      case h::_ => Some (h)
    } }

  // The variants for lists in the book code use fork, but I think they need
  // that only because they would like to parallelize list elemetn execution.
  // In this case we do not need to parallelize - one thread is enough.
  // Whatever parallelization is needed in producing the single value of Par[A]
  // should be applied by the user at earlier stages.
}



/*** Par[Option[_]] vs Option[Par[_]]
 *
 * Q11. (5%)
 *
 * Explain in English what does the function from Q10 achieve? (provide its user
 * oriented description, not an explanation of the implementation).
 */

 // Write here ...

 // This function takes a computation whose construction might have failed (the
 // computation might not be there, be triviallly None), and rewraps it into a
 // computation that produces an optional value A.  This allows composing the
 // result into a larger parallel computation expression.



object Q12 {

  /*** Monads
   *
   * Recall the Identity Monad, the simplest possible monad, which allows unit
   * (identity) and mapping (function application) on any type.
   */

  type Id[A] = A

  implicit val idMonad = new Monad[Id] {

    def unit[A] (a: => A): Id[A] = a
    def flatMap[A,B] (a: Id[A]) (f: A => Id[B]): Id[B] = f(a)
  }

  /* Now assume that there is a function loop of the following type. */

  // def loop[A, M[_]: Monad] (initial: M[A]) (body: A => A) (p: A => Boolean): M[A] = ???

  /* This function runs a loop 'in the monad M'.  It starts at the initial
   * value, then it maps the initial value to the output of the function 'body'
   * as long as the produced value of the type 'A' satisfies the predicate 'p'.
   *
   *
   * The code below computes a sum of list of integers.
   * (Warning: imperative code below)
   *
   * def sum (var l: List[Int]): Int = {
   *   var result = 0
   *   while (l.nonEmpty) {
   *     result = result + l.head
   *     l = l.tail
   *   }
   *   return result
   * }
   *
   * Q12. (10%)
   *
   * Convert the above imperative implementation of 'sum' into a pure one by
   * using the 'loop' function and the identity monad.  The implementation has
   * been started for you.  Complete it by replacing ??? (you may uncomment the
   * code):
   */

  // def sum (l: List[Int]): Int = {

  //   val initial: (List[Int], Int) = ???
  //   val body = ???
  //   val p = ???

  //   val result = loop[???,???] (initial) (body) (p)
  //   result._2
  // }

  // the import from Q13 is needed to test the solution
  import adpro.SOLUTIONS.Q13.loop

  // An example solution
  type LI = (List[Int], Int)

  def sum (l: List[Int]): Int = {

    val initial: LI = (l, 0)
    val body: LI => LI = { case (l, result) => (l.tail, result + l.head) }
    val p: LI => Boolean = { case (l, result) => l.nonEmpty }

    val result: LI = loop[LI,Id] (initial) (body) (p)
    result._2
  }

  /* DISCLAIMER: Normally, we do not want to compute a sum of a list in this way.
   * This is an artificial exercise for simplicity.
   */
}

object Q13 {

  /*** Looping in a Monad
   *
   * Q13. (10%)
   *
   * Implement the function 'loop' from the above exercise that iterates a
   * calculation in a monad. Given an initial value of type A it checks (like a
   * while loop) whether it satisfies the predicate p. If not it returns the
   * value.
   */

  // This solution is fine, but we do not have extension methods
  // implemented that delegate M[A].flatMap and
  // M[A].unit to Monad.flatMap and Monad.unit. So this would not compile. I
  // would still award full points, as this is a reasonable course of action,
  // which would work out in a real library like Cats.
  //
  // def loop_ [A, M[_]: Monad] (initial: M[A]) (body: A => A) (p: A => Boolean): M[A] =
  //   for {
  //     a <- initial
  //     result <- if (p(a)) loop_ (implicitly[Monad[M]].unit (body (a))) (body) (p) else initial
  //   } yield result

  // this solution compiles
  def loop [A, M[_]: Monad] (initial: M[A]) (body: A => A) (p: A => Boolean): M[A] =
    implicitly[Monad[M]].flatMap (initial) { a =>
      if (p (a))
        loop (implicitly[Monad[M]].unit (body(a))) (body) (p)
      else initial // or: implicitly[Monad[M]].unit (a)
    }

}
