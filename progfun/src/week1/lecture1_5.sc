package week1

object lecture1_5 {

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0000000001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(1e60)                                      //> res0: Double = 1.0E30
  sqrt(1e-60)                                     //> res1: Double = 1.0E-30
  "ab" < "b"                                      //> res2: Boolean = true

  def factorial(n: Int): Int = {

    def factorialTail(n: Int, actual: Int): Int =
      if (n == 1) actual
      else factorialTail(n - 1, actual * n)

    factorialTail(n, 1)
  }                                               //> factorial: (n: Int)Int
  
  factorial(5)                                    //> res3: Int = 120

}