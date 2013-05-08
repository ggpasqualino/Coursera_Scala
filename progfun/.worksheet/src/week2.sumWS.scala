package week2

object sumWS {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(196); 
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) 0
      else loop(a + 1, acc + a)
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(26); val res$0 = 
  
  sum((a) => a)(1, 2);System.out.println("""res0: Int = """ + $show(res$0))}
}
