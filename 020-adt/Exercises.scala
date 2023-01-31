// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

//> using target { scope "main" }
//> using scala "3.1.3"
//> using options "-Xfatal-warnings", "-deprecation", "-feature", "-source:future"

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(_ , tail) => tail

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] = 
    if n<=0 then l
    else
      l match
        case Nil => throw NoSuchElementException()
        case Cons(h, t) => drop(t,n-1)

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = 
      l match
        case Nil => l
        case Cons(h, t) if p(h) => dropWhile(t,p)
        case _  => l
  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = 
    l match
      case Nil => throw NoSuchElementException()
      case Cons(h, t) if t != Nil => Cons(h, init(t)) 
      case Cons(h, t) if t == Nil => Nil
      case _ => l

  // Exercise 6

  def length[A] (l: List[A]): Int = 
    foldRight(l, 0, (x,y) => 1+y)

  // Exercise 7
  @annotation.tailrec 
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
    l match
      case Nil => z
      case Cons(head, tail) => foldLeft(tail,f(z,head), f)

  // Exercise 8

  def product (as: List[Int]): Int = 
      foldLeft(as, 1, _ * _)

  def length1[A] (as: List[A]): Int = 
      foldLeft(as, 0, (x,y) => x+1)

  // Exercise 9
  def reverse[A] (l: List[A]): List[A] = 
      foldLeft(l, List[A](), (b, a) => Cons(a, b))
 
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = 
    foldLeft(reverse(l), z , (smth: B, smtha: A) => f(smtha,smth))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
      foldRight[A, B=>B](l, (b:B) => b, (a,g) => b => g(f(b,a)))(z)
 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = 
      foldLeft(l, List[A](), (b, a) => append(b,a))
 
  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = 
    l match
      case Cons(h, t) if p(h) => Cons(h, filter(t,p)) 
      case Cons(h, t) if !p(h) => filter(t,p)
      case _ => l
 
  // Exercise 14
  // 
  //  foldRight[A, List[B]] (l, List[A](), (a, z) => Cons(f(a), z))
  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = 
    foldLeft(l, List[B](), (b, a) =>  append(b,f(a)))

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = 
    flatMap(l, i => if p(i) then List(i) else Nil)

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = (l,r) match
      case (_ ,Nil) => Nil
      case (Nil ,_) => Nil
      case (Cons(h, t), Cons(h1, t1)) => Cons(h+h1, addPairwise(t,t1))

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = (l,r) match     
      case (_ ,Nil) => Nil
      case (Nil ,_) => Nil
      case (Cons(h, t), Cons(h1, t1)) => Cons(f(h,h1), zipWith(t,t1,f))

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = (sup,sub) match
      case (Nil ,Nil) => true
      case (_ ,Nil) => true
      case (Nil ,_) => false
      case (Cons(h, t), Cons(h1, t1)) if h == h1 => if hasSubsequence(t,t1) then true
                                                    else hasSubsequence(t,sub)
      case (Cons(h, t), Cons(h1, t1)) if h != h1 => hasSubsequence(t,sub)
      case _ => false
 

