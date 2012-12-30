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
    case Cons(0.0, _) => 0.0
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

}
