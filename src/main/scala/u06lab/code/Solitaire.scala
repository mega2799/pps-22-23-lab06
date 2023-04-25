package u06lab.code

type Card = (Int, Int)
val size = 5

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def computeMoves(c : Card) =
    Set(
      (c._1 - 2, c._2), (c._1 + 2, c._2), // horizontal
      (c._1, c._2 - 2), (c._1, c._2 + 2), // vertical
      (c._1 - 1, c._2 - 1), (c._1 + 1, c._2 + 1), // diagonal
      (c._1 - 1, c._2 + 1), (c._1 + 1, c._2 - 1) // diagonal
    )

//  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
//  println(render(solution = Seq((0, 0), (2, 1)), width = 5, height = 5))