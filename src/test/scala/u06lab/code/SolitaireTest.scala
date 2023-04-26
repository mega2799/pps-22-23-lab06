package u06lab.code

import org.junit.Test
import org.junit.Assert.*


class SolitaireTest:
  val width, height = 5
  def sol = Solitaire
  @Test
  def checkComputeMoves() =
    assertEquals(
      Set((0, 2), (1, 1), (1, 3), (2,0), (2, 4), (3, 1), (3, 3), (4, 2)),
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