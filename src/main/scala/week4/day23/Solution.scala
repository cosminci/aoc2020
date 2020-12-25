package week4.day23

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week4/day23/input.txt")
      .head
      .split("")
      .map(_.toInt)
      .toList

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 1", () => solvePartTwo(inputData))
  }

  private def solvePartOne(cups: List[Int]): String = {
    val nextCupPointers = Array.ofDim[Int](cups.length + 1)
    cups.indices.foreach { cupIndex =>
      nextCupPointers(cups(cupIndex)) = cups((cupIndex + 1) % cups.length)
    }
    play(nextCupPointers, cups.max, cups.head, rounds = 100)
    (0 until cups.length - 1)
      .foldLeft(("", nextCupPointers(1))) { case ((output, cupToAdd), _) =>
        (output + cupToAdd, nextCupPointers(cupToAdd))
      }
      ._1
  }

  private def play(nextCupPointers: Array[Int], maxCup: Int, firstCup: Int, rounds: Int) = {
    (1 to rounds).foldLeft(firstCup) { case (startingCup, _) =>
      val firstCupToMove  = nextCupPointers(startingCup)
      val secondCupToMove = nextCupPointers(firstCupToMove)
      val thirdCupToMove  = nextCupPointers(secondCupToMove)
      val nextStartingCup = nextCupPointers(thirdCupToMove)
      nextCupPointers(startingCup) = nextStartingCup
      val cupsToMove = List(firstCupToMove, secondCupToMove, thirdCupToMove)
      val cupToMoveAfter = List(1, 2, 3, 4).collectFirst {
        case delta if startingCup - delta == 0 =>
          cupsToMove.sorted.reverse.foldLeft(maxCup) {
            case (currentMax, cupToMove) if cupToMove == currentMax => currentMax - 1
            case (currentMax, _)                                    => currentMax
          }
        case delta if !cupsToMove.contains(startingCup - delta) =>
          startingCup - delta
      }.get
      nextCupPointers(thirdCupToMove) = nextCupPointers(cupToMoveAfter)
      nextCupPointers(cupToMoveAfter) = firstCupToMove
      nextStartingCup
    }
    nextCupPointers
  }

  private def solvePartTwo(seedCups: List[Int]): Long = {
    val maxCup = 1000000

    val nextCupPointers = Array.ofDim[Int](maxCup + 1)
    seedCups.indices.foreach { cupIndex =>
      nextCupPointers(seedCups(cupIndex)) = seedCups((cupIndex + 1) % seedCups.length)
    }
    nextCupPointers(seedCups.last) = seedCups.max + 1
    (seedCups.max + 1 until maxCup).foreach { cup =>
      nextCupPointers(cup) = cup + 1
    }
    nextCupPointers(maxCup) = seedCups.head

    play(nextCupPointers, maxCup, seedCups.head, rounds = 10000000)

    nextCupPointers(1).toLong * nextCupPointers(nextCupPointers(1))
  }

}
