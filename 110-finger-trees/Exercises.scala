// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using options "-deprecation", "-feature", "-source:future", "-language:adhocExtensions"

package adpro.fingerTrees

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.*

import adpro.monoid.Monoid.monoid
import adpro.monoid.Monoid

import java.util.NoSuchElementException

/** The type class for reducible structures. */
trait Reduce[F[_]]: 

  extension [A](as: F[A])
    def reduceR[B](opr: (A, B) => B)(b: B): B
    def reduceL[B](opl: (B, A) => B)(b: B): B

  /** We also provide a global 'static-function' interface for flexibility. 
    * This allows us to anambiguously call this method when extensions
    * conflict with class' own methods, for instance 'toList'. */
  def reduceR[A, B](opr: (A, B) => B)(fa: F[A], b: B): B = fa.reduceR(opr)(b)
  def reduceL[A, B](opl: (B, A) => B)(b: B, fa: F[A]): B = fa.reduceL(opl)(b)

  object laws:
    def reducersEquivalentForMonoids[A: Monoid](using Arbitrary[F[A]]): Prop =
      forAll { (fa: F[A]) =>
        val M = monoid[A]
        fa.reduceR[A](M.combine)(M.empty) == fa.reduceL[A](M.combine) (M.empty)
      }
 
end Reduce


/** A helper function allowing to find given instances of Reduce */
def reduce[F[_]: Reduce]: Reduce[F] = summon[Reduce[F]]


// Exercise 7

given Reduce[List] = new:
      extension [A] (as : List[A])
        def reduceR[B](opr: (A,B) => B)(b : B) : B = as.foldRight(b)(opr)
        def reduceL[B](opL: (B,A) => B)(b: B) : B = as.foldLeft(b)(opL)

end given

// Exercise 8

extension [F[_]: Reduce, A] (as: F[A])
  /** A convenience alias to distinguish from the standard lib toList */
  def toListFromReduce: List[A] = toList

  def toList: List[A] = 
    as.reduceR[List[A]]((a : A, l : List[A]) =>  a :: l)(Nil : List[A])

end extension


enum Node[+A]:
  case Node2(l: A, r: A)
  case Node3(l: A, m: A, r: A)

end Node


type Digit[+A] = List[A]

object Digit: 
  /** A factory method that allows us to use Digit(...) like a constructor */
  def apply[A](as: A*): Digit[A] = List(as*)

  /** An example of extractor, allowing us to use Digit(...) in pattern
    * matching.  Case classes and enums get extractors automatically, but Digit
    * defined as above is not a case class, but just a type alias. */
  def unapplySeq[A](d: Digit[A]): Option[Seq[A]] = Some(d)

end Digit


enum FingerTree[+A]:
  /** pr - prefix, m - middle, sf - suffix */
  case Deep(pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
  case Single(data: A)
  case Empty


  def empty = this match
    case Empty => true
    case _     => false

  def nonEmpty = !empty

end FingerTree

import FingerTree.*, Node.*, Digit.*

// Exercise 10

given Reduce[Node] = new:
  extension [A] (as : Node[A])
    def reduceR[B](opr: (A,B) => B)(z : B) : B = as match
      case Node2(a,b) => opr(a, opr(b, z))
      case Node3(a,b,c) => opr(a, opr(b, opr(c, z)))
    def reduceL[B](opL: (B,A) => B)(z: B) : B = as match
      case Node2(a,b) => opL(opL(z, a), b)
      case Node3(a,b,c) => opL(opL(opL(z, a), b), c)

end given


// Exercise 11 

// Nedas Solution (Doesnt work for Booleans + strings cuz ???)

/*given Reduce[FingerTree] = new:
  extension [A] (as : FingerTree[A])
    def reduceR[B](opr: (A,B) => B)(z : B) : B = as match
      case FingerTree.Empty => z
      case Single(data) => opr(data,z)
      case Deep(pr, m, sf) => 
        reduce[Digit].reduceR(opr)(pr,
        reduce[FingerTree].reduceR(reduce[Node].reduceR(opr))(m,
        reduce[Digit].reduceR(opr)(sf,z)
        ))
        //
    def reduceL[B](opL: (B,A) => B)(z: B) : B = as match
      case FingerTree.Empty => z
      case Single(data) => opL(z,data)
      case Deep(pr, m, sf) => 
        reduce[Digit].reduceL(opL)(
          reduce[FingerTree].reduceL(reduce[Node].reduceL(opL))
            (reduce[Digit].reduceL(opL)(z
              ,sf)
            ,m)
        ,pr)

end given */

// Niclas solution

given Reduce[FingerTree] = new:
    self =>
    extension [A] (t : FingerTree[A])
      def reduceR[B](opr: (A,B) => B)(b : B) : B = t match
        case Empty => b
        case Single(x) => opr(x, b)
        case Deep(pr, m, sf) =>
          val reduceDigit = reduce[Digit].reduceR(opr)
          val reduceTree = self.reduceR(reduce[Node].reduceR(opr))
            reduceDigit(pr, reduceTree(m, reduceDigit(sf, b)))

      def reduceL[B](opL : (B,A) => B)(b : B) : B = t match
        case Empty => b
        case Single(x) => opL(b ,x)
        case Deep(pr, m, sf) =>
          val reduceDigit = reduce[Digit].reduceL(opL)
          val reduceTree = self.reduceL(reduce[Node].reduceL(opL))
          reduceDigit(reduceTree(reduceDigit(b, pr), m), sf)
end given 


// Exercise 12 (page 5)

extension [A](t: FingerTree[A])
  def addL(a: A): FingerTree[A] = t match
    case Empty => Single(a)
    case Single(b) => Deep(Digit(a), Empty, Digit(b))
    case Deep(Digit(b, c, d, e), m, sf) =>  Deep(Digit(a, b), m.addL(Node3(c, d, e)), sf)
    case Deep(pr, m, sf) => Deep(a :: pr, m , sf)

  def addR(a: A): FingerTree[A] = t match
    case Empty => Single(a)
    case Single(b) => Deep(Digit(b), Empty, Digit(a))
    case Deep(pr, m, Digit(e, d, c, b)) => Deep(pr, m.addR(Node3(e, d, c)), Digit(b, a))
    case Deep(pr, m, sf) => Deep(pr, m, sf ++ Digit(a))

// Mark where are the polymorphically recursive calls

// Extensions so that we can call addL and addR as static methods as well
extension (companion: FingerTree.type)
  def addL[A](a: A, t: FingerTree[A]): FingerTree[A] = t.addL(a)
  def addR[A](t: FingerTree[A], a: A): FingerTree[A] = t.addR(a)


object Exercise13Spec
  extends org.scalacheck.Properties("ex13.."):

  // Exercise 13 (addR, tests to be written)

  property("Ex13.01: addR produces a deque containing the inserted element") =
    Empty.addR(42).toList == List(42)

  property("Ex13.02: addR produces a deque containing two inserted elements") =
    Empty.addR(42).addR(43).toList == List(42,43)

  property("Ex13.03: addR folded over a list gives a deque containing its elems") =
    forAll(Gen.listOfN(100, Gen.choose(0, 1000))) { (l: List[Int]) =>
      l.foldLeft[FingerTree[Int]](Empty)(FingerTree.addR).toList == l
    }

end Exercise13Spec 


// Exercise 14

extension [A](t: FingerTree[A])
  def ▷(a: A): FingerTree[A] = t.addR(a)

extension [A](a: A)
  def ◁(t: FingerTree[A]): FingerTree[A] = t.addL(a)


// Exercise 15

extension [F[_]: Reduce, A] (fa: F[A])
  def toTree: FingerTree[A] =
    // <' = reducer(<)
    fa.reduceR((f : A, a : FingerTree[A]) =>  f ◁ a)(Empty : FingerTree[A])

// Exercise 16  

def fingerTreeOfN[A: Arbitrary](n: Int): Gen[FingerTree[A]] =
    for l <- Gen.listOfN[A] (n, summon[Arbitrary[A]].arbitrary)
    yield l.toTree

def fingerTree[A: Arbitrary]: Gen[FingerTree[A]] =
  for
      n <- Gen.choose(3, 10)
      t <- fingerTreeOfN[A](n)
  yield t


given [A: Arbitrary]: Arbitrary[FingerTree[A]] = Arbitrary(fingerTree[A])

// Exercise 17 

object Exercise17Spec
  extends org.scalacheck.Properties("ex17.."):

  // Exercise 17 (Reduce[FingerTree])

  property("Ex17.01: Test Reduce[FingerTree] laws on intAddition monoid") =
    reduce[FingerTree].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intAddition)

  property("Ex17.02: Test Reduce[FingerTree] laws on intMultiplication") =
    reduce[FingerTree].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intMultiplication)

  property("Ex17.03: Test Reduce[FingerTree] laws on stringMonoid") =
    reduce[FingerTree].laws.reducersEquivalentForMonoids[String]
      (using Monoid.stringMonoid)

  property("Ex17.04: Reduce[FingerTree] laws on the monoid of List[Boolean]") =
    reduce[FingerTree].laws.reducersEquivalentForMonoids[List[Boolean]]
      (using Monoid.listMonoid[Boolean])

end Exercise17Spec


// Exercise 18

// In the paper views are generic in the type of tree used. Here I make them
// fixed for FingerTrees.

enum ViewL[+A]:
  case NilDeque 
  case ConsL(hd: A, tl: FingerTree[A])


// Exercise 19 (page 6, using Scala extractors)

def deepL[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] =
  if pr.size > 0 then Deep(pr,m,sf)
  else m match
    case NilDeque() => sf.toTree
    case ConsL(a,m1) => Deep(a.toList,m1,sf) 

def deepR[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] =
  if sf.size > 0 then Deep(pr,m,sf)
  else m match
    case NilDeque() => pr.toTree
    case ConsR(m1,a) => Deep(pr, m1, a.toList.reverse) 

/** Warning: A little  counterintuitively, pattern matching on NilDeque,
  * requires a parentheses as in: 
  *
  * case NilDeque() => 
  * 
  * I do not fully understand why. */

object NilDeque:
  def unapply[A](t: FingerTree[A]): Boolean =
    t.empty

object ConsL:
  def unapply[A](t: FingerTree[A]): Option[(A, FingerTree[A])] = t match
      case FingerTree.Empty => None
      case Single(x) => Some(x, Empty.toTree)
      case Deep(pr, m, sf) => Some(pr.head, deepL(pr.tail,m,sf))
    
object ConsR:
  def unapply[A](t: FingerTree[A]): Option[(FingerTree[A], A)] = t match
      case FingerTree.Empty => None
      case Single(x) => Some(Empty.toTree, x)
      case Deep(pr, m, sf) => Some(deepR(pr, m, sf.tail), sf.head)

/** Easy to use convenience wrappers around matchers.
  *
  * They are used in tests */
extension [A](t: FingerTree[A])
  def headL: A = t match 
    case ConsL(h, _) => h
    case _ => throw NoSuchElementException() 

  def tailL: FingerTree[A] = t match
    case ConsL(_,t) => t
    case _ => throw NoSuchElementException()

  def headR: A = t match
    case ConsR(_,h) => h
    case _ => throw NoSuchElementException()

  def tailR: FingerTree[A] = t match
    case ConsR(t,_) => t
    case _ => throw NoSuchElementException()

// vim:cc=80:conceallevel=2
