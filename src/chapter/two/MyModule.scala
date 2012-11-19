package chapter.two

import chapter.two.FunctionLiterals.absolute
import chapter.two.FunctionLiterals.divisibleBy
import chapter.two.FunctionLiterals.lessThan

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

    /**
     * EXCERCISE 2
     * Using absolute to get absolute value of a function
     */
    val twoTimes: Int => Int = n => n * 2
    assert(twoTimes(-3) == -6)
    val absTwoTimes = absolute(twoTimes)
    assert(absTwoTimes(-3) == 6)

    /**
     * EXERCISE 4
     * Using divisbleBy to check whether a given number is divisible by
     * another number k
     */
    val divisibleByTwo = divisibleBy(2)
    assert(divisibleByTwo(8) == true)
    assert(divisibleByTwo(412) == true)
    assert(divisibleByTwo(13) == false)
    
    val divisibleByFive = divisibleBy(5)
    assert(divisibleByFive(6) == false)
    assert(divisibleByFive(15) == true)
    
    /**
     * EXERCISE 5
     * Using the divisibleBy-based even function to test divisibility by 2
     */
    assert(even(2) == true)
    assert(even(5) == false)
    assert(even(11) == false)
    assert(even(10) == true)
  }
}