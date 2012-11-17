package chapter.two

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val s = "The absolute value of %d is %d."
    s.format(x, abs(x))
  }

  /** Program entry point */
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))

    /** Using the lessThan function from FunctionLiterals */
    import chapter.two.FunctionLiterals._
    val b = lessThan(10, 20)
    println(b)
  }
}