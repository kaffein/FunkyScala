package chapter.three.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nasoloaina
 * Date: 1/24/13
 * Time: 3:03 PM
 * To change this template use File | Settings | File Templates.
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * EXERCISE 26
   * Counting the number of nodes in a tree
   */
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  /**
   * EXERCISE 27
   * Writing the max function for Tree
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(n) => n
  }

  /**
   * EXERCISE 28
   * Writing depth function for Tree
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + (depth(left) max depth(right))
    case Leaf(_) => 0
  }

  /**
   * EXERCISE 29
   * Writing map function for Tree
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(_) => Leaf(f(_))
  }

  /**
   * EXERCISE 30
   * Implementing fold function for Tree
   *
   * Implementing size, maximum, depth and map via fold
   */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = l match {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(_) => f(_)
  }

  def size_[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)
  def maximum_(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depth_[A](t: Tree[A]): Int = fold(t)(a => 0)(1 + (_ max _))
  def map_[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))




}