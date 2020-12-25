package week2.day9

import cats.syntax.all._

import scala.annotation.tailrec

object Solution {

  def isSummable(preamble: List[Long], targetSum: Long): Boolean = {
    utils
      .cartesianProductFromList(preamble, targetSum)
      .exists { case (x, y) =>
        x + y == targetSum
      }
  }

  def solvePartOne(preambleSize: Int, numbers: List[Long]): Option[Long] = {
    val (preamble, tail) = numbers.splitAt(preambleSize)
    tail
      .foldM(preamble) { case (preamble, number) =>
        if (isSummable(preamble, number)) (preamble.tail :+ number).asRight else number.asLeft
      }
      .swap
      .toOption
  }

  def solvePartTwo(number: Long, numbers: List[Long]): Long = {
    val range       = findRangeWithSum(Range(min = 0, max = 1), number, numbers.toArray)
    val numberRange = (range.min to range.max).map(numbers)
    numberRange.min + numberRange.max
  }

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfLongs("week2/day9/input.txt")
    utils.timeSolution("Part 1", () => solvePartOne(preambleSize = 25, inputData))
    solvePartOne(preambleSize = 25, inputData).foreach { invalidNumber =>
      utils.timeSolution("Part 2", () => solvePartTwo(invalidNumber, inputData))
    }
  }

  @tailrec
  private def findRangeWithSum(range: Range, targetSum: Long, numbers: Array[Long]): Range = {
    (range.min to range.max).map(numbers).sum match {
      case `targetSum`            => range
      case sum if sum > targetSum => findRangeWithSum(Range(range.min + 1, range.max), targetSum, numbers)
      case _                      => findRangeWithSum(Range(range.min, range.max + 1), targetSum, numbers)
    }
  }

  case class Range(min: Int, max: Int)

}
