package u06lab.code

import org.junit.Test
import org.junit.Assert.*


class SolitaireTest:
  val width, height = 5
  def sol = Solitaire
  @Test
  def checkComputeMoves() =
  // some moves are out of the board but this tests only the function
    assertEquals(
      Set((5,2), (0,4), (-1,2), (2,-1), (2,5), (4,4), (4,0), (0,0)),
      sol.computeMoves((2,2))
    )

  @Test
  def outOfBOund() =
    assertTrue(
      sol.inBoardPosition(5,5)((1,1))
    )
    assertTrue(
      sol.inBoardPosition(5,5)((3, 4))
    )
    assertFalse(
      sol.inBoardPosition(5,5)((-1, 1))
    )
    assertFalse(
      sol.inBoardPosition(5,5)((3, 5))
    )

  @Test
  def luigilambertTest() =
    println("board 3 * 5 => 13272 solutions")
    assertEquals(
      13272,
      sol.placeMarks(5, 7)(35).size
    )
    
//  def init() =
//    assertEquals(
//      Set((1,1)),
//      sol.init()
//    )
