// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.lazyList

// Note: we are using our own lazy lists, not the standard library

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => t()

  /* Note 1. f can return without forcing the tail
   *
   * Note 2. this is not tail recursive (stack-safe) It uses a lot of stack if
   * f requires to go deeply into the lazy list. So folds sometimes may be less
   * useful than in the strict case
   *
   * Note 3. We added the type C to the signature. This allows to start with a
   * seed that is a subtype of what the folded operator returns.
   * This helps the type checker to infer types when the seed is a subtype, for 
   * instance, when we construct a list:
   *
   * o.foldRight (Nil) ((a,z) => a:: z)
   *
   * The above works with this generalized trick. Without the C generalization
   * the compiler infers B to be List[Nothing] (the type of Nil) and reports
   * a conflict with the operator.  Then we need to help it like that:
   *
   * o.foldRight[List[Int]] (Nil) ((a,z) => a:: z)
   *
   * With the C type trick, this is not neccessary. As it hints the type
   * checker to search for generalizations of B.
   *
   * I kept the foldLeft type below in a classic design, so that you can
   * appreciate the difference. Of course, the same trick could've been
   * applied to foldLeft.
   */
  def foldRight[B, C >: B](z: => B)(f: (A, => C) => C): C = this match
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))

  /* Note 1. Eager; cannot be used to work with infinite lazy lists. So
   * foldRight is more useful with lazy lists (somewhat opposite to strict lists)
   * Note 2. Even if f does not force z, foldLeft will continue to recurse.
   */
  def foldLeft[B](z: => B)(f :(A, => B) => B): B = this match
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)

  // Note: Do you know why we can implement find with filter for lazy lists but
  // would not do that for regular lists?
  def find(p: A => Boolean) = 
    this.filter(p).headOption

  // Exercise 2

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h,t) => h() :: (t().toList)
  

  // Test in the REPL, for instance: LazyList(1,2,3).toList 
  // (and see what list is constructed)

  // Exercise 3

  def take(n: Int): LazyList[A] = this match
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case Cons(h,t) if n == 0 => Empty
    case _ => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case Cons(h,t) if n == 0 => cons(h(), t())
    case _ => Empty
  

  // Exercise 4

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty

  // Exercise 5
  
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
 
  // Note 1. lazy; tail is never forced if satisfying element found this is
  // because || is non-strict
  // Note 2. this is also tail recursive (because of the special semantics
  // of ||)
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 6
  
  def takeWhile1(p: A => Boolean): LazyList[A] = 
    foldRight(empty)((a, b) => if p(a) then cons(a,b) else empty)
  // Exercise 7
  
  def headOption1: Option[A] = 
    foldRight(None)((a, b) => Some(a))

  // Exercise 8

  // Note: The type is incorrect, you need to fix it
  def map[B] (f:  A => B): LazyList[B] = 
    foldRight(Empty)((a, b) => cons(f(a), b))

  // Note: The type is incorrect, you need to fix it
  def filter(p:  A => Boolean): LazyList[A] = 
    foldRight(empty[A])((a, b) => if p(a) then cons(a,b) else b)


  /* Note: The type is given correctly for append, because it is more complex.
   * Try to understand the type. The contsraint 'B >: A' requires that B is a
   * supertype of A. The signature of append allows to concatenate a list of
   * supertype elements, and creates a list of supertype elements.  We could have
   * writte just the following:
   *
   * def append(that: => LazyList[A]): LazyList[A]
   *
   * but this would not allow adding a list of doubles to a list of integers
   * (creating a list of numbers).  Compare this with the definition of
   * getOrElse last week, and the type of foldRight this week.
   */
  def append[B >: A](that: => LazyList[B]): LazyList[B] = 
    foldRight(that)((a, b) => cons(a,b) )

  // Note: The type is incorrect, you need to fix it
  def flatMap[B]( f: A => LazyList[B]): LazyList[B] = 
    foldRight(Empty)((a, b) => f(a).append(b))

  // Exercise 9
  // 
  //def find(p: A => Boolean): Option[A]= this.filter(p).headOption

  // Lazy list do not have a whole list initialised but rather are split up
  // into trunks that must be forced to execute before we get a value.
  //
  // In this case find uses a lazy list filter that utalises foldright to filter
  // out values. However, it does not actually filter out values but assign a series of instructions to 
  // the trunks. Thus, by analysing an instruction set we can determine if a filter
  // will ever get an expected value and thus we can terminate it earlier.
  // If there is no value, we can also terminate the execution earlier thus it
  // is more efficiant to use that to find values in lazy list.
  // In normal list. All the values are evaluated using the filter command.
  // Thus it would take a longer time to execute because you cant terminate a regular filter.
  // earlier.
  //
  // Scroll down to Exercise 10 in the companion object below

  // Exercise 13

  def mapUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): LazyList[A] =
    unfold((this , n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): LazyList[A] =
      unfold((this , p)) {
        case (Cons(h, t), p) if p(h()) => Some((h(), (t(), p)))
        case _ => None
      }

  def zipWith[B >: A, C](ope: (=> B, => B) => C)(bs: LazyList[B]): LazyList[C] =
      unfold((this, bs , ope)) {

        case ( _ , Empty, ope) =>
          None
        case (Empty, _ , ope) =>
          None
        case (Cons(h1, t1), Cons(h2, t2), ope) =>
          Some(ope(h1(),h2()), (t1(),t2(), ope))

      }

end LazyList // enum ADT



// The companion object for lazy lists ('static methods')

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))

  // Exercise 1

  def from(n: Int): LazyList[Int] =
    cons(n, from(n+1))

  def to(n: Int): LazyList[Int] =
    cons(n, to(n-1))

  lazy val naturals: LazyList[Int] =
    from(1)

  // Scroll up to Exercise 2 to the enum LazyList definition 
  
  // Exercise 10

  // Note: The type is incorrect, you need to fix it
  lazy val fibs: LazyList[Int] = 
    def fib_tail(a: Int, b: Int): LazyList[Int] = 
      cons(a, fib_tail(b, a + b))
  
    fib_tail(0 , 1)


  // Exercise 11

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
        f(z).map( (a, b) => cons(a, unfold(b)(f)) ).getOrElse(empty)

    // f match
      /*case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty*/

  // Exercise 12

  // Note: The type is incorrect, you need to fix it
  lazy val fibsUnfold: LazyList[Int] = 
    unfold((0 , 1)) {
     (a,b) => Some(a,(b, a + b))
    }

  // Scroll up for Exercise 13 to the enum

end LazyList // companion object
