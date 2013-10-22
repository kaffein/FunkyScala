package chapter.two


object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val s = "The absolute value of %d is %d."
    s.format(x, abs(x))
  }

  /** Program entry point */
  def main(args: Array[String]) {
    assert(formatAbs(-42).equalsIgnoreCase("The absolute value of -42 is 42."))

    /** Using the lessThan function from FunctionLiterals */
    import chapter.two.FunctionLiterals._
    val b = lessThan(10, 20)
    assert(b == true)

    /**
     * EXCERCISE 2
     * Using absolute to get absolute value of a function
     */
    val twoTimes: Int => Int = n => n * 2
    assert(twoTimes(-3) == -6)
    val absTwoTimes = absolute(twoTimes)
    assert(absTwoTimes(-3) == 6)

    val sortedDoubleArray: Array[Double] = Array(0d, 5d, 6d, 7d, 10d)
    assert(isSorted(sortedDoubleArray, (x: Double, y: Double) => x > y) == true)
    val notSortedIntArray: Array[Int] = Array(4, 6, 8, 2)
    assert(isSorted(notSortedIntArray, (x: Int, y: Int) => x > y) == false)


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
    
    /**
     * EXERCISE 6
     * Using the polymorphic lift higher-order function to check whether
     * a number is divisible by 3 OR 5, or by 3 AND 5
     */
    val divisibleByThree = divisibleBy(3)
    val divisibleByThreeAndFive = polymorphicLift.lift[Int,Boolean,Boolean,Boolean]((x, y) => x && y)(divisibleByThree, divisibleByFive)
    val divisibleByThreeOrFive = polymorphicLift.lift[Int,Boolean,Boolean,Boolean]((x, y) => x || y)(divisibleByThree, divisibleByFive)
    
    assert(divisibleByThreeAndFive(15) == true)
    assert(divisibleByThreeAndFive(18) == false)
    
    assert(divisibleByThreeOrFive(3) == true)
    assert(divisibleByThreeOrFive(5) == true)
    assert(divisibleByThreeOrFive(18) == true)
    assert(divisibleByThreeOrFive(25) == true)
    assert(divisibleByThreeOrFive(7) == false)

    /**
     * EXERCISE 12
     * Using tail-recursion-powered fibonacci function
     */
    assert(fib(6) == 8)
    assert(fib(9) == 34)
    assert(fib(12) == 144)
    assert(fib(17) == 1597)
    assert(fib(20) == 6765)

    /**
     * EXERCISE 13
     * Using tail-recursion-powered Newton's method to find square
     * roots
     */
    assert(sqrt(4.0) == 2.0)
    assert(sqrt(16.0) == 4.0)
    assert(sqrt(81.0) == 9.0)
    assert(sqrt(25.0) == 5.0)
  }
}