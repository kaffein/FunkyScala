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


}