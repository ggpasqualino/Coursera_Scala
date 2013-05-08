package week2

object lesson2_1 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }
    loop(a, 1)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  def operation(f: Int => Int, u: (Int, Int) => Int, initialAcc: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, u(acc, (f(a))))
    }
    loop(a, initialAcc)
  }                                               //> operation: (f: Int => Int, u: (Int, Int) => Int, initialAcc: Int)(a: Int, b:
                                                  //|  Int)Int

  sum((a) => a)(1, 2)                             //> res0: Int = 3
  product((a) => a)(1, 2)                         //> res1: Int = 2
  def factorial(n: Int) = product(a => a)(1, n)   //> factorial: (n: Int)Int
  
  val x = new Rational(1,3)                       //> x  : week2.Rational = 1/3
  val y = new Rational(5,7)                       //> y  : week2.Rational = 5/7
  val z = new Rational(3,2)                       //> z  : week2.Rational = 3/2
	x.sub(y).sub(z)                           //> res2: week2.Rational = -79/42
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