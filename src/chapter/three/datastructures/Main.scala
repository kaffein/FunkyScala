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


  }

}
