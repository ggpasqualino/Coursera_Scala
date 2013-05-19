package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(3, 9)), "3,9")
      assert(!terrain(Pos(8, 5)), "8,5")
      assert(!terrain(Pos(9, 5)), "9,5")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar S level 1") {
    new Level1 {
      assert(startPos === Pos(1, 1))
    }
  }

  test("findChar T level 1") {
    new Level1 {
      assert(goal === Pos(4, 7))
    }
  }

  test("startBlock isLegal") {
    new Level1 {
      assert(startBlock.isLegal)
    }
  }

  test("block not legal") {
    new Level1 {
      assert(!(new Block(Pos(2, 0), Pos(3, 0)).isLegal))
    }
  }

  test("startBlock isStanding") {
    new Level1 {
      assert(startBlock.isStanding)
    }
  }

  test("block not standing") {
    new Level1 {
      assert(!(new Block(Pos(2, 0), Pos(3, 0)).isStanding))
    }
  }

  test("startBlock neighbors") {
    new Level1 {
      val bLeft = Block(Pos(1, -1), Pos(1, 0))
      val bRight = Block(Pos(1, 2), Pos(1, 3))
      val bUp = Block(Pos(-1, 1), Pos(0, 1))
      val bDown = Block(Pos(2, 1), Pos(3, 1))
      assert(startBlock.neighbors === List((bLeft, Left), (bRight, Right), (bUp, Up), (bDown, Down)))
    }
  }

  test("startBlock legal neighbors") {
    new Level1 {
      val bRight = Block(Pos(1, 2), Pos(1, 3))
      val bDown = Block(Pos(2, 1), Pos(3, 1))
      assert(startBlock.legalNeighbors === List((bRight, Right), (bDown, Down)))
    }
  }

  test("done") {
    new Level1 {
      assert(done(new Block(Pos(4, 7), Pos(4, 7))))
    }
  }

  test("not done") {
    new Level1 {
      assert(!done(new Block(Pos(1, 1), Pos(1, 1))))
    }
  }

  test("neighbors with history") {
    new Level1 {
      val neighbors = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet === neighbors)
    }
  }

  test("new neighbors only") {
    new Level1 {
      val neighbors = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
        
      val visited = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))

      val newNeighbors = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
      assert(newNeighborsOnly(neighbors, visited) === newNeighbors)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
