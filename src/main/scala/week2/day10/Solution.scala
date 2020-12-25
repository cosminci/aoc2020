package week2.day10

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfIntegers("week2/day10/input.txt")
    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  def solvePartOne(numbers: List[Int]): Int = {
    val sorted = numbers.sorted
    val (oneDiffs, threeDiffs, _) = (0 +: sorted).foldRight((0, 0, sorted.last + 3)) {
      case (n, (oneDiffCount, threeDiffCount, prevN)) =>
        val difference        = prevN - n
        val newOneDiffCount   = oneDiffCount + (if (difference == 1) 1 else 0)
        val newThreeDiffCount = threeDiffCount + (if (difference == 3) 1 else 0)
        (newOneDiffCount, newThreeDiffCount, n)
    }
    oneDiffs * threeDiffs
  }

  def solvePartTwo(numbers: List[Int]): Option[Long] = {
    val sorted = numbers.sorted
    sorted
      .foldLeft(Map(0 -> 1L)) { case (counts, n) =>
        val nCount = counts.getOrElse(n - 1, 0L) + counts.getOrElse(n - 2, 0L) + counts.getOrElse(n - 3, 0L)
        counts + (n -> nCount)
      }
      .get(sorted.last)
  }

}
