package chapter.two

object FunctionLiterals {

  val max: (Int, Int) => Int = (a, b) => if (a > b) a else b

  /**
   * EXERCISE 1
   * Using the underscore notation to define functions
   */
  val lessThan: (Int, Int) => Boolean = _ < _

  /** Defines a function that negates another function */
  def not(p: Int => Boolean): Int => Boolean = n => !(p(n))
  def even(n: Int): Boolean = n % 2 == 0
  def negative(n: Int): Boolean = n < 0

  /** Defining odd and positive using the not function */
  val odd = not(even)
  val positive = not(negative)

  /**
   * EXERCISE 2
   * Using higher-order function to define absolute
   */
  def abs(n: Int): Int = if(n < 0) -n else n
  def absolute(f: Int => Int): Int => Int = n => abs(f(n))

}