package chapter.two

object FunctionLiterals {

  val max: (Int, Int) => Int = (a, b) => if (a > b) a else b

  /**
   * EXERCISE 1
   * Using the underscore notation to define functions
   */
  val lessThan: (Int, Int) => Boolean = _ < _
  
  /** Defines a polymorphic function that negates another function */
  // def not(p: Int => Boolean): Int => Boolean = n => !(p(n))
  def not[A](p: A => Boolean): A => Boolean = n => !(p(n))
  // def even(n: Int): Boolean = n % 2 == 0
  def negative(n: Int): Boolean = n < 0

  /** Defining odd and positive using the not function */
  val odd = not(even)
  val positive = not(negative)

  /**
   * EXERCISE 2
   * Using higher-order function to define absolute
   */
  def abs(n: Int): Int = if(n < 0) -n else n
  // def absolute(f: Int => Int): Int => Int = n => abs(f(n))
  
  /**
   * EXERCISE 3
   * Making absolute polymorphic
   */
  def absolute[A](f: A => Int): A => Int = n => abs(f(n))
  
  /**
   * EXERCISE 4
   * Writing a method divisibleBy that returns a predicate checking
   * whether a given number is divisible by k
   */
  type Pred[A] = A => Boolean
  
  def divisibleBy(k: Int): Pred[Int] = n => n % k == 0
  
  /**
   * EXERCISE 5
   * Rewriting even so that it uses divisibleBy
   */
  def even(n: Int) = divisibleBy(2)(n)
  
  /**
   * EXERCISE 6
   * Writing a function to check divisibility using higher-order
   * functions
   */
  // def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = n => f(g(n), h(n))
  def lift[A,B,C,D](f: (B,C) => D)(g: A => B, h: A => C): A => D = n => f(g(n), h(n))
  
  /**
   * EXERCISE 7
   * Implementing an abstract higher-order function that performs currying and another
   * that performs uncurrying
   */
  def curry[A,B,C](f:(A,B) => C): A => B => C = a => b => f(a,b) 
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)
  
  /**
   * EXERCISE 8
   * Implementing a higher-order function that composes two functions
   * Equivalent to : f compose g OR g andThen f
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}