package u06lab.code

import org.junit.Test
import org.junit.Assert.*


class SolitaireTest:
  def sol = Solitaire
  @Test
  def checkComputeMoves() =
    assertEquals(
      Set((0, 2), (1, 1), (1, 3), (2,0), (2, 4), (3, 1), (3, 3), (4, 2)),
      sol.computeMoves((2,2))
    )

  def init() =
    assertEquals(
      Set((1,1)),
      sol.init()
    )