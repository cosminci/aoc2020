package week1.day5

object Solution {

  private val MaxRowRange: Range    = Range(0, 127)
  private val MaxColumnRange: Range = Range(0, 7)

  def main(args: Array[String]): Unit = {
    val inputData: List[SeatLocator] = utils.loadInputAsListOfStrings("week1/day5/input.txt").map(parseInput)

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[SeatLocator]): Int = {
    inputData.map(calculateSeatId).max
  }

  private def solvePartTwo(inputData: List[SeatLocator]): Option[Int] = {
    val occupiedSeats = inputData.map(calculateSeatId).sorted
    val shiftedLeft   = occupiedSeats.tail :+ occupiedSeats.head
    occupiedSeats
      .zip(shiftedLeft)
      .find { case (id1, id2) =>
        id2 - id1 == 2
      }
      .map(_._2 - 1)
  }

  private def parseInput(input: String) = {
    val (rowLocator, columnLocator) = input.toCharArray.splitAt(7)
    SeatLocator(rowLocator, columnLocator)
  }

  private def calculateSeatId(seatLocator: SeatLocator): Int = {
    val row    = locatePosition(seatLocator.row, MaxRowRange, 'F', 'B')
    val column = locatePosition(seatLocator.column, MaxColumnRange, 'L', 'R')
    row * 8 + column
  }

  private def locatePosition(locator: Array[Char], maxRange: Range, moveDownSentinel: Char, moveUpSentinel: Char) = {
    locator
      .foldLeft(maxRange) { case (range, direction) =>
        direction match {
          case `moveDownSentinel` =>
            Range(range.min, (range.max + range.min) / 2)
          case `moveUpSentinel` =>
            Range((range.max + range.min + 1) / 2, range.max)
        }
      }
      .min
  }

  case class SeatLocator(row: Array[Char], column: Array[Char])

  case class Range(min: Int, max: Int)
}
