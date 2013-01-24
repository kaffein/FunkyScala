package chapter.three.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nasoloaina
 * Date: 12/30/12
 * Time: 1:29 AM
 * To change this template use File | Settings | File Templates.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /* Extractor */
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    // case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /**
   * EXERCISE 2
   * Implementing the tail function which removes the first element
   * on the List data structure
   */
  def tail[A](ds: List[A]): List[A] = ds match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  /**
   * EXERCISE 3
   * Generalize tail to the function drop which removes the first n
   * elements of a list
   */
  def drop[A](l: List[A])(n: Int): List[A] = {

    def loop(l: List[A], n: Int): List[A] = if (n == 0) l else loop(tail(l), n - 1)

    loop(l, n)

  }

  /**
   * EXERCISE 4
   * Implementing dropWhile on List[A] data structure
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {

      def loop(l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(x, xs) => if (f(x)) loop(xs, f) else l
        case Nil => l
      }

      loop(l, f)

  }

  /**
   * EXERCISE 5
   * Implementing setHead to replace the first element of a
   * list
   */
  def setHead[A](l: List[A])(a: A) = l match {
    case Cons(x, xs) => Cons(a, xs)
    case Nil => Cons(a, Nil)
  }

  /**
   * EXERCISE 6
   * Implementing init which returns all but the last element of
   * a list
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   * Recursion over lists and generalization over
   * higher-order functions
   */
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)
  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  /**
   * EXERCISE 10
   * Compute the length of a list using foldRight
   */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  /**
   * EXERCISE 11
   * Implementing foldLeft as another general list-recursion function
   * that is tail-recursive (unlike foldRight)
   */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  /**
   * EXERCISE 12
   * Implementing sum, product and length using foldLeft
   */
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
   * EXERCISE 13
   * Implementing reverse using fold
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x, xs) => Cons(xs, x))

  /**
   * EXERCISE 15
   * Implementing append with foldRight
   */
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  /**
   * EXERCISE 17
   * Implementing a function that transforms a list of integers by
   * adding 1 to each element
   */
  def add1(l: List[Int]) = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  /**
   * EXERCISE 18
   * Implementing a function that transforms each value in a list of double into
   * String
   */
  def double2String(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  /**
   * EXERCISE 19
   * Implementing a function that generalizes modifying each element in a list
   * while maintaining the list structure
   */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
   * EXERCISE 20
   * Implementing a function that filters/removes elements from a list unless they
   * satisfy a given predicate
   */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => if (!f(x)) filter(xs)(f) else Cons(x, filter(xs)(f))
  }

  /**
   * EXERCISE 21
   * Implementing a flatmap function that works like a map except that the function
   * given will return a list instead of a single result and that list should be
   * inserted into the final resulting list
   */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  /**
   * EXERCISE 22
   * Implementing filter using flatMap
   *
   */
  def filter_[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  /**
   * EXERCISE 24
   * Implementing zip which allows pairing elements of
   * two different lists
   */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

}