// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n*n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))
    println(square(6))

end MyModule

// Exercise 2 requires no programming
// Exercise 3

def f(n: Int): Int = 
    @annotation.tailrec
    def printF(n: Int): Int = 
      if n % 2 == 0 then println(n)
      if n == 0 then n
      else printF(n-1)
      
    printF(2*n)

// Exercise 3

def fib(n: Int): Int = 
    @annotation.tailrec
    def fib_tail(n: Int, a: Int, b: Int): Int = 
      if n == 0 then a
      else fib_tail(n - 1, b, a + b)
  
    fib_tail(n-1, 0 , 1) 


// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  if as.isEmpty then true else
    @annotation.tailrec 
    def checkOrder(currentPosition: Int): Boolean =
      if currentPosition+1 == as.length then true
      else if ordered(as(currentPosition),as(currentPosition+1)) then checkOrder(currentPosition+1)
      else false
    checkOrder(0)
// Exercise 5
def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => ((b:B) => f(a,b))

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
    (a: Array[A]) => (b: (A, A) => Boolean) =>   
      if a.isEmpty then true else
        @annotation.tailrec 
        def checkOrder(currentPosition: Int): Boolean =
          if currentPosition+1 == a.length then true
          else if b(a(currentPosition),a(currentPosition+1)) then checkOrder(currentPosition+1)
          else false
        checkOrder(0)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a) (b)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
    (a: Array[A], b: (A, A) => Boolean) =>   
      if a.isEmpty then true else
        @annotation.tailrec 
        def checkOrder(currentPosition: Int): Boolean =
          if currentPosition+1 == a.length then true
          else if b(a(currentPosition),a(currentPosition+1)) then checkOrder(currentPosition+1)
          else false
        checkOrder(0)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
