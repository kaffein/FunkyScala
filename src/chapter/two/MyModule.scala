package chapter.two

import com.sun.tools.corba.se.idl.constExpr.LessThan

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val s = "The absolute value of %d is %d."
    s.format(x, abs(x))
  }

  /** Program entry point */
  def main(args: Array[String]): Unit = {
    assert(formatAbs(-42).equalsIgnoreCase("The absolute value of -42 is 42."))

    /** Using the lessThan function from FunctionLiterals */
    import chapter.two.FunctionLiterals._
    val b = lessThan(10, 20)
    assert(lessThan(10, 20) == true)
    
    /** Using absolute to get absolute value of a function */
    val twoTimes: Int => Int = n => n * 2
    assert(twoTimes(-3) == -6)
    val absTwoTimes = absolute(twoTimes)
    assert(absTwoTimes(-3) == 6)
  }
}