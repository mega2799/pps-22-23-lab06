package u06lab.code

type Card = (Int, Int)
type Solution = Iterable[Card]
type IterableFactory = Solution => Iterable[Solution]
given IterableFactory = Seq(_).view
val size = 5
val deckSize = size * size

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

  def renderSolution(solution: (Seq[(Int, Int)], Int) , width: Int, height: Int): String =
    val reversed = solution._1.reverse
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

  def inBoardPosition(width : Int = size, height: Int = size)(c : Card) : Boolean =
    (c._1 >= 0 && c._1 < width) && (c._2 >= 0 && c._2 < height)

  def placeMarks(width : Int = size, height : Int = size)(n : Int = deckSize)(using factory : IterableFactory) : Iterable[Solution] =
    n match
      case 0 => factory(Set((width/2 , height/2)))
      case _ =>
        for
          placedCards <- placeMarks()(n - 1)
          y <- 0 to size
          x <- 0 to size
          card=(x, y)
//          _ = Console.println("serie: %d \n computing: %s with res: %b".format(n, card, computeMoves(placedCards.last) filter(inBoardPosition()) contains(card)))
//          _ = Console.println(render(placedCards.toSeq, size, size))
          _ = Console.println("\n")
          _ = Console.println(placedCards)
//            _ = Console.println(placedCards.last)
//            _ = Console.println("\n")
          if computeMoves(placedCards.last) filter(inBoardPosition()) contains(card)
        yield
          placedCards.toSeq :+ card

//  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
//  println(render(solution = placeMarks()().view, width = 5, height = 5))

//  placeMarks()() foreach()
  placeMarks(3, 3)(9).zipWithIndex foreach((el, index) => println("for solution %d there are %d sol".format(index, el.size)))

//  println(placeMarks()().size)

