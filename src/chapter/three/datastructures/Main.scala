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

    /**
     * EXERCISE 6
     * Assertion for testing init method which returns all but the last
     * element of a list
     */
    assert(List.init(List(2, 4, 6, 8, 10, 11)) == List(2, 4, 6, 8, 10))
    assert(List.init(List(2, 4, 6)) == List(2, 4))
    // assert(List.init(Nil) == "init of empty list")

    /**
     * EXERCISE 9
     * See what happens when you pass Nil and Cons themselves to foldRight,
     * like	this:	foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
     */
    //List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    /**
     * EXERCISE 10
     * Compute the length of a list using foldRight
     */
    assert(List.length(List(1,3,3))  == 3)
    assert(List.length(List(3, 3, 3, 5, 9)) == 5)
    assert(List.length(List(6, 8)) == 2)
    assert(List.length(List(1, 9, 10, 4)) == 4)


    /**
     * EXERCISE 11
     * Asserting foldLeft results
     */
    assert(List.foldLeft(List(1,3,4), 0)(_ + _) == 8)
    assert(List.foldLeft(List(1,3,4), 1)(_ * _) == 12)

    /**
     * EXERCISE 12
     * Implementing sum, product and length using foldLeft
     */
    assert(List.sum3(List(1,2,3)) == 6)
    assert(List.sum3(List(2,4,9)) == 15)

    assert(List.product3(List(1,2,3)) == 6)
    assert(List.product3(List(2,3,6)) == 36)

    assert(List.length2(List(1,3,4)) == 3)
    assert(List.length2(List(3,5)) == 2)

    /**
     * EXERCISE 13
     * Asserting reverse results
     */
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(List.reverse(List.reverse(List(1, 2, 3))) == List(1, 2, 3))

    /**
     * EXERCISE 15
     * Asserting append results
     */
    assert(List.append(List(1, 2, 3), List(1, 2)) == List(1, 2, 3, 1, 2))

    /**
     * EXERCISE 17
     * Asserting add1 results
     */
    assert(List.add1(List(1, 2, 3)) == List(2, 3, 4))
    assert(List.add1(List(4, 5, 6)) == List(5, 6, 7))

    /**
     * EXERCISE 18
     * Asserting double2String function results
     */
    assert(List.double2String(List(4, 5, 6)) == List("4.0", "5.0", "6.0"))

    /**
     * EXERCISE 20
     * Asserting on filter function results
     */
    assert(List.filter(List(2, 4, 6, 8, 9))(_ % 2 == 0) == List(2, 4, 6, 8))
    assert(List.filter(List(3, 6, 9))(_ % 3 == 0) == List(3, 6, 9))

    assert(List.filter_(List(2, 4, 6, 8, 9))(_ % 2 == 0) == List(2, 4, 6, 8))
    assert(List.filter_(List(3, 6, 9))(_ % 3 == 0) == List(3, 6, 9))

    /**
     * EXERCISE 21
     * Asserting flatMap results
     */
    def mapper(x: Int) = List(x - 1, x, x + 1)
    assert(List.flatMap(List(2, 4))(mapper(_)) == List(1, 2, 3, 3, 4, 5))

  }

}
