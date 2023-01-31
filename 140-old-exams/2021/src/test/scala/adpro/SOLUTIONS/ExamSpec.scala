/* These tests have not been available for the students writing the exam. */
package adpro.SOLUTIONS

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class Exam2020AutumnSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.Inside
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "Part 1" - {

    "A test that always passes (a sanity check)" in {
      forAll { (n: Int) => n shouldBe n }
    }

  }


  "Q1 (Warm-up hello)" in {

    import Q1._

    hello (Triangle) should be ("triangle")
    hello (Triangle: Printable) should be ("triangle")
    hello (Square) should be ("square")
    hello (Square: Printable) should be ("square")
    hello (new Printable {}) should be ("printable")

  }

  "Q2 (sequence and either)" in {

    import Q2._

    sequence (List()) should be (Right(List()))
    sequence (List (Left (()))) should be (Left (()))
    sequence (List (Left (1), Left (2))) should be (Left (2))

    forAll { l: List[Int] =>
      sequence (l map { Right (_) }) should be (Right (l))
    }

  }

  "Q3 (Typing)" in {

    """
      def test[Err, A, F[_]: adpro.Foldable] =
        Q3.sequence[Err, A, F] _ // should compile
    """ should compile
  }


  "Q4 (Random Value Generators)" in {

    import adpro._
    import adpro.SOLUTIONS.Q4._
    import scala.math.Ordering.Double
    import scala.math.Ordering.Int

    forAll { seed: Long =>
      val (l, r, x) = riid.run (RNG.Simple (seed))._1
      l should be <= r
      l.toDouble should be <= x
      x should be <= (r.toDouble)
    }
  }


  "Q5 (Type Extensions)" in {

    import adpro._
    import SOLUTIONS.Q5._
    import SOLUTIONS.Q4.riid

    // this should just type check
    "lazy val r = RNG.map (riid) _" should compile
  }


  "Q8 (A better size for streams)" in {

    import adpro.Stream._

    val s = cons (???, cons(???, adpro.Stream.empty))
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan (s) (0) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan (s) (1) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan (s) (2) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan (s) (3) should be (false)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan_ (s) (0) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan_ (s) (1) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan_ (s) (2) should be (true)
    adpro.SOLUTIONS.Q8.checkIfLongerEqThan_ (s) (3) should be (false)

  }


  "Q10 (Your Parallel Options)" in {

    info ("No tests implemented! This is falsely green!")
  }

  "Q12 (Monads) (also tests Q13)" in {

    forAll { l: List[Int] =>
      adpro.SOLUTIONS.Q12.sum (l) should be (l.sum) }
  }



}


class Q9MySpec extends Q9.MySpec
