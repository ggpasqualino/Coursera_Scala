package week1

object lecture1_5 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(81); 

  def abs(x: Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(322); 

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0000000001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(15); val res$0 = 

  sqrt(1e60);System.out.println("""res0: Double = """ + $show(res$0));$skip(14); val res$1 = 
  sqrt(1e-60);System.out.println("""res1: Double = """ + $show(res$1));$skip(13); val res$2 = 
  "ab" < "b";System.out.println("""res2: Boolean = """ + $show(res$2));$skip(186); 

  def factorial(n: Int): Int = {

    def factorialTail(n: Int, actual: Int): Int =
      if (n == 1) actual
      else factorialTail(n - 1, actual * n)

    factorialTail(n, 1)
  };System.out.println("""factorial: (n: Int)Int""");$skip(19); val res$3 = 
  
  factorial(5);System.out.println("""res3: Int = """ + $show(res$3))}

}
