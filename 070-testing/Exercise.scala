// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

 // import lazyList00.* // uncomment to test the book laziness solution implementation
  import lazyList01.* // uncomment to test the broken headOption implementation
  // import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*
import adpro.lazyList.lazyList01.LazyList.apply
import scala.util.Try

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)

/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2 headOption does not force the tail of a lazy list
  property("Ex02.01: headOption does not force the tail of LazyList") = 
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    //If it forced tail. It would crash due to dividing by zero exception
    forAll {(s: LazyList[Int], n : Int) => cons(n, s.map( _/0)).headOption; true }
  
  // Exercise 3 Test that take does not force any heads nor any tails of the lazy list it manipulates
  property("Ex03.01: take does not force heads nor any tails") = 

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    //If it forced head or tail. It would crash due to dividing by zero exception
    forAll {(s: LazyList[Int], n : Int)  => ((s.map( _/0)).take(n)).isInstanceOf[LazyList[Int]] }

  property("Ex03.02: If we force to take. The list should throw an exception") = 
      val s = LazyList(1)
      try {
          ((s.map( _/0)).take(10).toList).isInstanceOf[List[Int]]
          false
      } catch {
          case e: ArithmeticException => true
          case _: Throwable => false
      }

  // Exercise 4 In this and the in the following exercises, the numbers n and m are assumed to be
  // non-negative. Test that take(n) does not force the (n+1)st head ever (even if we force all elements
  // of take(n)).

  property("Ex04.01: take(n) does not force the (n+1)st head ever (even if we force all elements of take(n)") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    //If it forced head or tail. It would crash due to dividing by zero exception
    forAll {(s: LazyList[Int], n : Int) => ((cons(n, s.map( _/0))).take(1).toList).isInstanceOf[List[Int]] }

  property("Ex04.02: if we would force n+1 head. We would get an exception.") = 
      val s = LazyList(1)
      val n = 1
      try {
          ((cons(n, s.map( _/0))).take(n+1).toList).isInstanceOf[List[Int]]
          false
      } catch {
          case e: ArithmeticException => true
          case _: Throwable => false
      }
  
  // Exercise 5 Test that l.take(n).take(n) == l.take(n) for any lazy list s and any n.
  property("Ex05.01:  l.take(n).take(n) == l.take(n) for any lazy list s and any n") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    // We must force a list here. Otherwise, we will recieve just a lazy list and its not comparable.
    forAll {(l: LazyList[Int], n : Int) => ((l.take(n).take(n)).toList) == l.take(n).toList }
  
  // Exercise 6 Test that l.drop(n).drop(m) == l.drop(n+m) for any n, m.
  property("Ex06.01:  l.drop(n).drop(m) == l.drop(n+m) for any n, m.") = 
    given Arbitrary[Int] = Arbitrary (Gen.choose(1, 20))
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    // We must force a list here. Otherwise, we will recieve just a lazy list and its not comparable.
    forAll {(l: LazyList[Int], n : Int, m: Int) => ((l.drop(n).drop(m)).toList) == (l.drop(n+m)).toList }
  
  // Exercise 7 Test that l.drop(n) does not force any of the dropped elements (heads). This should hold even if we force some element in the tail.
  property("Ex07.01: l.drop(n) does not force any of the dropped elements (heads)") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    //If it forced head. It would crash due to dividing by zero exception
    forAll {(l: LazyList[Int], n : Int) => ((l.map( _/0)).drop(n)).isInstanceOf[LazyList[Int]] }

  property("Ex07.02: This should hold even if we force some element in the tail") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    //If it forced head. It would crash due to dividing by zero exception
    forAll {(l: LazyList[Int], n: Int) => (n > 0) ==> ((cons(n/0, l)).drop(n).toList.isInstanceOf[List[Int]])}

  // Exercise 8 Test that l.map(identity) == l for any lazy list l. Here identity is the identity function.
  property("Ex08.01: Test that l.map(identity) == l for any lazy list l") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int]) => l.map(identity).toList == l.toList }

  // Exercise 9 Test that map terminates on infinite lazy lists.
  property("Ex09.01: Test that map terminates on infinite lazy lists.") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])
    forAll { (l: LazyList[Int]) => {
        l.map(identity) 
        true
      }
    }
 
  // Exercise 10 Test correctness of append on the properties you formulate yourself, understanding what the function should be doing.

  property("Ex10.01: append should be a combination of two lists") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int], s: LazyList[Int]) => l.append(s).toList == l.toList ::: s.toList}

  property("Ex10.02: append should not force both lists") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])
    forAll { (l: LazyList[Int], s: LazyList[Int]) => (l.map(_/0).append(s.map(_/0))).isInstanceOf[LazyList[Int]] }

