package chapter.two

case class Box(height: Double, width: Double)

object Box {

  /**
   * Defines comparison in terms of pure functions
   * The greaterBy function accepts a pure function (Box => Double) and this function
   * returned values are used to do the comparison
   */
  def greaterBy(x: Box, y: Box, f: Box => Double): Box = if (f(x) > f(y)) x else y

  /** Defines wider to be a comparison on the box width */
  def wider(x: Box, y: Box): Box = greaterBy(x, y, p => p.width)

  /** Defines wider to be a comparison on the box height */
  def taller(x: Box, y: Box): Box = greaterBy(x, y, p => p.height)
  
  /**Defines wider and taller using the underscore notation */
  val wider: (Box, Box) => Box = greaterBy(_, _, p => p.width)
  
  val taller: (Box, Box) => Box = greaterBy(_, _, p => p.height)
  
  /** Defines a function that negates another function */
  def not(p: Int => Boolean): Int => Boolean = n => !(p(n))
  def even(n: Int): Boolean = n % 2 == 0
  def negative(n: Int): Boolean = n < 0
  
  /** Defining odd and positive using the not function */
  val odd = not(even)
  val positive = not(negative)
}