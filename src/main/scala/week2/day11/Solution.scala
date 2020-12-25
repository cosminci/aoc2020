package week2.day11

import scala.annotation.tailrec

object Solution {

  type SeatMap        = Array[(Array[(Char, Int)], Int)]
  type AlreadyChecked = Array[Array[Map[XYDelta, Boolean]]]

  def solvePartOne(seatMap: SeatMap): Int = {
    updateSeatsUntilStable(seatMap, threshold = 4, countOccupiedNeighbours).map { case (row, _) =>
      row.count(seat => seat._1 == '#')
    }.sum
  }

  def solvePartTwo(seatMap: SeatMap): Int = {
    updateSeatsUntilStable(seatMap, threshold = 5, countOccupiedDirections).map { case (row, _) =>
      row.count(seat => seat._1 == '#')
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week2/day11/input.txt")
      .map(_.toCharArray.zipWithIndex)
      .toArray
      .zipWithIndex
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
    utils.timeSolution("Part 1", () => solvePartOne(inputData))
  }

  private def updateSeats(seatMap: SeatMap, threshold: Int, countOccupiedFn: SeatMap => (Int, Int) => Int): SeatMap = {
    def countOccupied = countOccupiedFn(seatMap)
    seatMap.map { case (row, rowIdx) =>
      (
        row.map {
          case ('L', colIdx) if countOccupied(rowIdx, colIdx) == 0 =>
            ('#', colIdx)
          case ('#', colIdx) if countOccupied(rowIdx, colIdx) >= threshold =>
            ('L', colIdx)
          case (status, colIdx) =>
            (status, colIdx)
        },
        rowIdx
      )
    }
  }

  @tailrec
  private def updateSeatsUntilStable(
      seatMap: SeatMap,
      threshold: Int,
      countOccupiedFn: SeatMap => (Int, Int) => Int
  ): SeatMap = {
    val updatedMap = updateSeats(seatMap, threshold, countOccupiedFn)
    if (seatStatuses(updatedMap) sameElements seatStatuses(seatMap)) seatMap
    else updateSeatsUntilStable(updatedMap, threshold, countOccupiedFn)
  }

  private def seatStatuses(updatedMap: SeatMap) = {
    updatedMap.flatMap(_._1.map(_._1))
  }

  case class XYDelta(x: Int, y: Int)

}
