package chapter.two

object FunctionLiterals {
  
  val max:(Int, Int) => Int = (a, b) => if(a > b) a else b
  
  /** Using the underscore notation to define functions */
  val lessThan: (Int, Int) => Boolean = _ < _
  
}