package chapter.two

object FunctionLiterals {

  val max: (Int, Int) => Int = (a, b) => if (a > b) a else b

  /**
   * EXERCICE 1
   * Using the underscore notation to define functions
   */
  val lessThan: (Int, Int) => Boolean = _ < _

}