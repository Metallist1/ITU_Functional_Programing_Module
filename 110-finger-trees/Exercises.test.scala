// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"

package adpro.fingerTrees

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import adpro.monoid.Monoid

object ReduceSpec
  extends org.scalacheck.Properties("reduce"):

  // Excercise 7 (reduceList)

  property("Ex07.01: Left-right reducers are the same for monoids [Int, +]") =
    reduce[List].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intAddition)

  property("Ex07.02: Left-right reducers are the same for monoids [Int, *]") =
    reduce[List].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intMultiplication)

  property("Ex07.03: Left-right reducers the same for monoids [String, +]") =
    reduce[List].laws.reducersEquivalentForMonoids[String]
      (using Monoid.stringMonoid)

  property("Ex07.04: Left-right reducers same for monoids [List[Boolean],++]") =
    reduce[List].laws.reducersEquivalentForMonoids[List[Boolean]]
      (using Monoid.listMonoid[Boolean])


  // Exercise 8 (toList)

  property("Ex08.01: Reduce[List[Int]].toList is identity") =
    given Monoid[Int] = Monoid.intAddition
    forAll { (l: List[Int]) =>
      l.toListFromReduce == l
    }

end ReduceSpec

import Node.*

// TOOD: it seems that we get arbitraries for enums for free 
// and do not really need this. but cannot get this to work. Check source
def genNode2[A: Arbitrary]: Gen[Node2[A]] =
  arbitrary[(A, A)].map(Node2.apply)

def genNode3[A: Arbitrary]: Gen[Node3[A]] = 
  arbitrary[(A, A, A)].map(Node3.apply)

def genNode[A: Arbitrary]: Gen[Node[A]] =
  Gen.oneOf(genNode2[A],genNode3[A])

given arbNodeA[A: Arbitrary]: Arbitrary[Node[A]] = 
  Arbitrary(genNode[A])

object NodeSpec
  extends org.scalacheck.Properties("node.."):

  // Exercise 10 (Reduce[Node])
  property("Ex10.01: Reduce[Node] reducers collapse for monoids Int+") =
    reduce[Node].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intAddition)

  property("Ex10.02: Reduce[Node] reducers collapse for monoid Int*") =
    reduce[Node].laws.reducersEquivalentForMonoids[Int]
      (using Monoid.intMultiplication)

  property("Ex10.03: Reduce[Node] reducers collapse for string monoid") =
    reduce[Node].laws.reducersEquivalentForMonoids[String]
      (using Monoid.stringMonoid)

  property("Ex10.04: Reduce[Node] reducers collapse for list monoid") =
    reduce[Node].laws.reducersEquivalentForMonoids[List[Boolean]]
      (using Monoid.listMonoid)

end NodeSpec


object FingerTreeSpec
  extends org.scalacheck.Properties("finger"):

  import FingerTree.*

  // Exercise 11 (simple regressions to ensure minimal progress)
  // (the real tests are done in Ex 17)

  val t = Deep(Digit(41), Empty, Digit(42))
  val l = List(41, 42)

  property("Ex11.01: A simple reducer regression (1)") = 
    t.toList == l

  property("Ex11.02: A simple reducer regression (2)") =
    t.reduceR[List[Int]] { _:: _ } (Nil) == l


  // Exercise 12 (addL)

  property("Ex12.01: produce a queue containing the inserted element") =
    Empty.addL(42).toList == List(42)

  property("Ex12.02: two consecutive addL get the order right (regression)") =
    Empty.addL(42).addL(41).toList == List(41, 42)

  property("Ex12.03: There is one way to represent <41,42> and addL complies") =
    Empty.addL(42).addL(41) == Deep(Digit(41), Empty, Digit(42))

  property("Ex12.04: produce a queue containing the inserted elements") =
    forAll(Gen.listOfN(100, Gen.choose(0, 1000))) { (l: List[Int]) =>
      l.foldRight[FingerTree[Int]](Empty)(FingerTree.addL).toList == l
    }

  // Exercise 14 (▷ & ◁)

  property("Ex14.01: ▷ produces a deque containing the inserted element") =
    (Empty ▷ 42).toList == List(42)

  property("Ex14.02: ▷ produces a deque containing two inserted elements") =
    (Empty ▷ 42 ▷ 43).toList == List(42, 43)

  property("Ex14.03: ◁ produces a deque containing the inserted element") =
    (42 ◁ Empty).toList == List(42)

  property("Ex14.04: ◁ produces a deque containing two inserted elements") =
    (42 ◁ (43 ◁ Empty)).toList == List(42, 43)

  // Exercise 15 (toTree)

  property("Ex15.01: toTree and toList should be inverses on List") =
    forAll { (l: List[Int]) => l.toTree.toList == l }
  

  // Exercise 19 (extractors)

  property("Ex19.01: Nil on Empty") =
    Empty match
      case NilDeque() => true
      case _ => false

  property("Ex19.02: ConsL(42,Nil) on Single") = Single(42) match
    case ConsL(42, NilDeque()) => true
    case _ => false 

  val ft3plus = for
    n <- Gen.choose(3, 100)
    t <- fingerTreeOfN[Int](n)
  yield t

  property("Ex19.03: ConsL(_,Consl(_,_)) should match any tree larger than 3") =
    forAll(ft3plus) { (t: FingerTree[Int]) =>
      t.toList.size >= 3 ==> { t match
       case ConsL(a, ConsL (b,_)) => true 
       case _ => false
    } } 
    
  property("Ex19.04: Test the the left prefix of a folded addL tree (tests addL via toTree)") =
    forAll { (l: List[Int]) =>
      l.toList.size >= 3 ==> {
        val t = l.toTree
        t.headL == l.head
        t.tailL.headL == l.tail.head
        t.tailL.tailL.headL == l.tail.tail.head
    } }

  property("Ex19.05: ConsR(ConsR(_,_),_) should match any tree larger than 3") =
    forAll(ft3plus) { (t: FingerTree[Int]) =>
      t.toList.size >= 3 ==> { t  match
       case ConsR(ConsR (_,_), _) => true 
       case _ => false
    } }

  property("Ex19.06: List (0,0,0).toTree regressions (deepL)") =
    val l = List (1,2,3)
    val t = l.toTree
    t.tailL.headL == l.tail.head && t.headR == 3 && t.headL == 1

  property("Ex19.07: Pattern should not be Nil on nonEmpty") =
    forAll { (t: FingerTree[Int]) =>
      t.nonEmpty ==> { t match
        case NilDeque() => false
        case _ => true
      } }

  property("Ex19.08: ConsR (Nil,_) should match on Single") =
    Single(42) match
      case ConsR(NilDeque (),42) => true
      case _ => false

  property("Ex19.09: it should have the right suffix on any tree larger than 3 (15)") =
    forAll { (l: List[Int]) =>
      l.size >= 3 ==> {
        val t = l.reverse.toTree
        { "the last element" |:  t.headR == l.head } &&
        { "2nd last element" |:  t.tailR.headR == l.tail.head }
    } }

  property("Ex19.10: List(0, 1, 0, 0, 0, 0) regression") =
    val l = List(2, 1, 0, 0, 0, 0)
    val t = l.reverse.toTree
    { "as 1st element" |: l.head == t.headR } &&
    { "as 2nd element" |: l.tail.head == t.tailR.headR }

end FingerTreeSpec

// vim:cc=80:conceallevel=2
