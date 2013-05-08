package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.LinkedList

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {

    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 20)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)

    val set1 = new Empty
    val set2 = set1.incl(a)
    val set3 = set2.incl(b)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val setABC = set1.incl(b).incl(a).incl(c)
    val setBCD = set1.incl(c).incl(b).incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def asList(tweets: TweetSet): LinkedList[Tweet] = {
    var res = LinkedList[Tweet]()
    tweets.foreach(tw => res = res :+ tw)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("size") {
    new TestSets {
      assert(asList(setABC).size === 3)
      assert(asList(setBCD).size === 3)
      setABC.union(setBCD).foreach(println)
      assert(asList(setABC.union(setBCD)).size === 4)
    }
  }

  ignore("Empty most retweeted") {
    new TestSets {
      println(set1.mostRetweeted)
    }
  }

  ignore("set5  most retweeted") {
    new TestSets {
      println(set5.mostRetweeted)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
//      trends.foreach(println)
    }
  }
}
