// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.par

import java.util.concurrent.{Executors, ExecutorService, Callable}

import scala.language.implicitConversions
import scala.io.Source


/* This non-blocking  version of Future (different from
 * java.util.concurrent) takes a continuation instead of returning a
 * value, and calls it when ready.
 */
opaque type Future[+A] = (A => Unit) => Unit

opaque type Par[A] = ExecutorService => Future[A]



/** This call, extracting the value will block the current thread on
  * latch.await. The users of API should not call it earlier than
  * needed. Run should be called as late as possible, and programming
  * should proceed using the other APIs to allow high concurrency (not
  * wasting a thread).
  */
extension [A] (pa: Par[A])

  def run(es: ExecutorService): A =
    val ref = java.util.concurrent.atomic.AtomicReference[A] ()
    val latch = java.util.concurrent.CountDownLatch(1) 
    pa (es) { a => ref.set (a); latch.countDown } 
    latch.await 
    ref.get

  def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es => k => 
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]] (es) {
        case Left (a) =>
          if br.isDefined then Par.eval(es)(k(f(a,br.get)))
          else ar = Some(a)
        case Right(b) =>
          if ar.isDefined then Par.eval(es)(k(f(ar.get,b)))
          else br = Some(b)
      }
      pa(es) { a => combiner ! Left (a)  }
      pb(es) { b => combiner ! Right (b) }

  def map[B](f: A => B): Par[B] =
    pa.map2(Par.unit (())) { (a, _) => f(a) }

  // Scroll below to Exercise 1, in object Par
  // Come back here after solving Exercise 7

  // Exercise 8

  def chooser[B](f: A => Par[B]): Par[B] = 
    es=>
       f(pa.run(es))(es)

  // Exercise 9 continues in the Par object, in the bottom of the file




object Par:

  /** Pass the value to the continuation, the executor service is not used. 
    * Note that unit is strict, so `a` will be computed in the current thread!
    */
  def unit[A](a: A): Par[A] =
    es => k => k (a)
  
  /** A non-strict version of `unit`, which may allow for parallelism. */
  def delay[A](a: => A): Par[A] =
    es => k => k (a)
  
  def fork[A](a: => Par[A]): Par[A] =
    es => k => eval (es) (a (es) (k))
  
  /** A helper function, for evaluating an action asynchronously, using
    * the given `ExecutorService`. This is just to avoid having to
    * instantiate a new Callable object every time we want to evaluate
    * something in this file. Taking an argument by-name is much
    * idomatic to scala (and lazy programming) than creating callable
    * objects (Java-style).  The ExecutorService is a Java API so we
    * create a small adaptation here to make things nice.
    */
  def eval(es: ExecutorService) (r: => Unit): Unit =
    es.submit (new Callable[Unit] { def call = r })
  
  def lazyUnit[A](a: => A): Par[A] = 
    fork(unit(a))

  // Exercise 1
  
  /* Write the answer here in a comment 
   * When we call by value, the compiler computes the passed expression once before calling the function.
   * However when we call it by name, it re-computes the expressions value each time it gets access inside the function.
   * We want seperate threads to evaluate the result, thus if we call it by value. The result will be already evaluated. 
   * Calling by name will not evaluate the expression before we put it on a seperate thread. This way we can achieve parallelism
   */
  
  // Exercise 2 
  
  def asyncF[A, B](f: A => B): A => Par[B] = 
    (a:A) => lazyUnit(f(a))
  
  // Exercise 3
  
  /* Write the answer here in a comment:
   * Test case is a a collection of test steps, expected results and data. 
   * Before we even consider testing we must define map and its functionality.
   * Our map function should take some Par[A] and remap its functionality to Par[B] given a series of instructions.
   * Map utalises map 2 which means that one side of the map will contain the list and all the values stored in it and the other will pass a no-op.
   * Par should not be evaluated for map to remap a value.
   * 
   * When designing test cases there is several methodologies you can follow. Which depends on what result we want to test and achieve. In this case we will focus on concurrency:
   * We will test for normal execution, upper and lower boundries (values that normally would not make sense to execute, but are possible) and incorrect upper and lower boundry values.
   * (values that should not produce results or make no sense.)
   * Thus, firstly we should simply pass a list of integers wrapped in Par from 0 to x and try to remap them to  _ + 1. We would create an another list start from 1 to x + 1 and wrap it in Par.
   * We then would compare the result from the map with the result of our second list. If it matches. The test would be sucessful.
   * Other test we would do : Pass and Empty list and map it to true. Pass a full list of values and map it to false and so on. 
   * for testing concurrency: We would check if the map does not force it to computate (we are mutating the value, not computing it). We would check if the map runs in parallel... ect.
   */
  
  // Exercise 4

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((a,b) => a.map2(b)(_::_))

  /* This is shown in the book: */
  def parMap[A, B](as: List[A]) (f: A => B): Par[List[B]] =
    sequence(as.map (asyncF (f)))

  // Exercise 5

  def wget(uris: String*): List[String] =  

      val es = java.util.concurrent.Executors.newFixedThreadPool (uris.size) : ExecutorService

      if uris.size <= 1 && uris.size > 0 then
        List(Source.fromURL(uris.apply(1))("ISO-8859-1").mkString)
      else if uris.size > 0 then
        List()
      else
        val (l, r) = uris.splitAt(uris.size / 2)
        val sumL: Par[List[String]] = unit(wget(l*))
        val sumR: Par[List[String]] = unit(wget(r*))
        sumL.run(es) ::: sumR.run(es)
    
  /* Write your explanation in English here:
   * Our approach uses recursion to divide work into small chunks.
   * Once we split divide the work, each half will call wget method recursively. 
   * This call will be wrapped in Par and will be unevaluated and will wait to be evaluated in a seperate thread.
   * Once we reach the smallest available chunk. We will call an time expensive method "fromURL" or we will return and empty list if there is no uris.
   * Lastly, we will now evaluate what each half of the split uris contains by running it in parallel using an executor service.
   * By executing both threads in parallel we do it concurrently 
   * Once evaluated, we finally concattinate both lists and output the result.
   */

  // Exercise 6

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      fork {
        val fbs: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else Nil))
        sequence(fbs).map(_.flatten)
      }


  // Exercise 7

  // Note: The solution from the text book does not apply immediately.
  // It has to be adapted to the non-blocking representation of Par and
  // Future that we are using in this chapter.

 /* def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    es=>
       choices.apply(pn.run(es))(es)
 
  def choice[A](pb: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(pb.map(a => if a then 0 else 1))(List(t,f))*/

  // Exercise 8 is found above, in the extension methods of Par[A]
  // Come back here when done with Exercise 8.
  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
      pn.chooser( i => choices.apply(i))
 
  def choice[A](pb: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      pb.chooser(i => if i then t else f)
  // Exercise 9
 
  def join[A](p: Par[Par[A]]): Par[A] =
    es => p.run(es)(es)
 
  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(p.map(f))

end Par




// Exercise 10

// The types here are broken, and you need to fix them
// replace THIS with a meaningful name
extension [A] (pa: Par[Par[A]]) 
  def join: Par[A] = 
    es => pa.run(es)(es)

// The types here are broken, and you need to fix them
extension [A] (pn: Par[Int]) 
  def choiceN (choices: List[Par[A]]): Par[A] = 
      pn.chooser( i => choices.apply(i))

// The types here are broken, and you need to fix them
extension [A] (pb: Par[Boolean]) 
  def choice (t: Par[A], f: Par[A]): Par[A] = 
      pb.chooser(i => if i then t else f)


/*
Explain briefly in English why all three of these extensions cannot be placed in the same extension
block? Why they cannot be put in the same block with extensions for Par[A]?
*/

/* Write your answer in English here:

 * In each of the three extension methods we need a diffrent type of Par value to execute them successfully.
 * In Join method, when we run Par, we need to get Par[A]. In ChoiceN, we need to get Int and in choice we need to get boolean.
 * If we would place all of them inside the same extension, we will get a type error each of the methods require a diffrent type to execute their instructions.
 * If we would place all of them in the same block with extensions for Par[A]. We would encounter the same issue as Par[A] would produce value A.
 * A != Int, Boolean or Par[A] thus a type error would be thrown.
 * 
 */

/* An example answer:
 *
 * The three above extension cannot be a part of the same module
 * extension, because they extend different types, respectively:
 * Par[Par[A]], Par[Int], and Par[Boolean]. For the same reason they
 * cannot be part of the extension of Par[A], as each of them requires
 * an object of a more specific type.
 */
