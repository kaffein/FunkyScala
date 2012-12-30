package chapter.three.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nasoloaina
 * Date: 12/30/12
 * Time: 1:41 AM
 * To change this template use File | Settings | File Templates.
 */
object Main {

  def main(args: Array[String]) {

    /**
     * EXERCISE 1
     * The pattern-matching result is 3
     */
    val res = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    assert(res == 3)

    /**
     * EXERCISE 2
     * Implementing tail function on List[+A] data structure
     */
    assert(List.tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
    assert(List.tail(Nil) == Nil)

    /**
     * EXERCISE 3
     * Implementing drop function using tail
     */
    assert(List.drop(List(1, 2, 3, 4, 5))(3) == List(4, 5))
    assert(List.drop(List(1, 2, 3, 4, 5))(2) == List(3, 4, 5))

    /**
     * EXERCISE 4
     * Implementing dropWhile on List[A] data structure
     */
    assert(List.dropWhile(List(1, 2, 3, 4, 5))(x => x < 4) == List(4, 5))
    assert(List.dropWhile(List(2, 4, 6, 8, 10, 11))(x => x % 2 == 0) == List(11))

    /**
     * EXERCISE 5
     * Assertion for testing setHead which replaces the first
     * element of a list
     */
    assert(List.setHead(List(2, 4, 6, 8, 10, 11))(3) == List(3, 4, 6, 8, 10, 11))
    assert(List.setHead(Nil)(3) == List(3))

  }

}
