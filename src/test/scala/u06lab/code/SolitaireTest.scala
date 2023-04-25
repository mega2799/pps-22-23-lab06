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
      sol.inBoardPosition()((1,1))
    )
    assertTrue(
      sol.inBoardPosition()((3, 4))
    )
    assertFalse(
      sol.inBoardPosition()((-1, 1))
    )
    assertFalse(
      sol.inBoardPosition()((3, 5))
    )
    
//  def init() =
//    assertEquals(
//      Set((1,1)),
//      sol.init()
//    )