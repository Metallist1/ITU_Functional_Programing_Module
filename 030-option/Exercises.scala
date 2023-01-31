// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.option


// Exercise 1
// if left larger +1 if right larger -1 if equal 0
trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int =
    if this.getY() == that.getY() then
      if this.getX() < that.getX() then -1
      else if this.getX() == that.getX() then 0
      else 1
    else
        if this.getY() < that.getY() then -1
        else 1 

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r) 

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Leaf(a) => a
    case Branch(l,r) => maximum(l).max(maximum(r))
  
  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Leaf(a) => g(a)
    case Branch(l,r) => f(fold(l)(f)(g), fold(r)(f)(g))

  def size1[A](t: Tree[A]): Int =  
    fold(t)((a: Int, b:Int) => a+b+1)(a => 1)

  def maximum1(t: Tree[Int]): Int = 
    fold(t)((a: Int, b:Int) => a.max(b))(a => a)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    fold[A, Tree[B]](t)((a, b) => Branch(a,b))(a => Leaf(f(a)))


enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] =  this match
    case Some(a) => Some(f(a))
    case None => None

  def getOrElse[B >: A] (default: => B): B = this match
      case Some(a) => a
      case None => default

  def flatMap[B](f: A => Option[B]): Option[B] = this match
      case Some(a) => f(a)
      case None => None

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(p: A => Boolean): Option[A] = this match
      case Some(a) if p(a) => Some(a)
      case _ => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)
    



object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    ao.flatMap(a => bo.map(b => f(a,b)))

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]](Some(Nil))((a,b) => map2(a,b) (_ :: _) )

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as.foldRight[Option[List[B]]](Some(Nil))((a,b) => map2(f(a),b)(_ :: _))
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] =
  headOption(lst).map( a => a._2)

def headGrade1(lst: List[(String,Int)]): Option[Int] =
  ???

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x-m, 2))))
 
// Scroll up, to the Option object for Exercise 9
