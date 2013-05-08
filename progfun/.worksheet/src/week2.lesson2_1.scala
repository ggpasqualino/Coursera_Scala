package week2

object lesson2_1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(205); 
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(177); 

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }
    loop(a, 1)
  };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(231); 

  def operation(f: Int => Int, u: (Int, Int) => Int, initialAcc: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, u(acc, (f(a))))
    }
    loop(a, initialAcc)
  };System.out.println("""operation: (f: Int => Int, u: (Int, Int) => Int, initialAcc: Int)(a: Int, b: Int)Int""");$skip(24); val res$0 = 

  sum((a) => a)(1, 2);System.out.println("""res0: Int = """ + $show(res$0));$skip(26); val res$1 = 
  product((a) => a)(1, 2);System.out.println("""res1: Int = """ + $show(res$1));$skip(48); 
  def factorial(n: Int) = product(a => a)(1, n);System.out.println("""factorial: (n: Int)Int""");$skip(31); 
  
  val x = new Rational(1,3);System.out.println("""x  : week2.Rational = """ + $show(x ));$skip(28); 
  val y = new Rational(5,7);System.out.println("""y  : week2.Rational = """ + $show(y ));$skip(28); 
  val z = new Rational(3,2);System.out.println("""z  : week2.Rational = """ + $show(z ));$skip(17); val res$2 = 
	x.sub(y).sub(z);System.out.println("""res2: week2.Rational = """ + $show(res$2))}
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  
  def neg() = new Rational(-x, y)

  override def toString = numer + "/" + denom
  
  def add(that: Rational) =
  	new Rational(
  		numer * that.denom + that.numer * denom,
  		denom * that.denom
  	)
  
  def sub(that: Rational) =
  	add(that.neg)
}
