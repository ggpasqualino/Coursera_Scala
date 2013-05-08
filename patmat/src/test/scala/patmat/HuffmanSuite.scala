package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Leaf('d', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), List('d', 'a', 'b'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('d', 'a', 'b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    assert(times(string2Chars("hello, world")) === List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton(List(Leaf('a', 2)))") {
    assert(singleton(List(Leaf('a', 2))))
  }

  test("singleton(List(Leaf('a', 2), Leaf('b', 1)))") {
    assert(singleton(List(Leaf('a', 2), Leaf('b', 1))) == false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until(listTree)") {
    new TestTrees {
      val listTree = List(Leaf('a', 2), Leaf('b', 3), Leaf('d', 4))
      assert(until(singleton, combine)(listTree) === List(t2))
    }
  }

  test("createCodeTree(ListTree)") {
    new TestTrees {
      val listTree = List('a', 'a', 'b', 'b', 'b', 'd', 'd', 'd', 'd')
      assert(createCodeTree(listTree) === t2)
    }
  }

  test("decode secret") {
    println(decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
