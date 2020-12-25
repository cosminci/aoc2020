package week1.day3

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week1/day3/input.txt").map(_.toCharArray).toArray

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 1", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: Array[Array[Char]]): Int = {
    val repeatWindow = inputData.head.length
    val xMovement    = 3
    inputData.zipWithIndex.count { case (row, rowNumber) =>
      val xPosition = (xMovement * rowNumber) % repeatWindow
      row(xPosition) == '#'
    }
  }

  private def solvePartTwo(slope: Array[Array[Char]]): Long = {
    List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    ).map { case (xMovement, yMovement) =>
      val everyYMovementRow = slope.grouped(yMovement).map(_.head).toArray
      countTrees(everyYMovementRow, xMovement).toLong
    }.product
  }

  private def countTrees(slope: Array[Array[Char]], xMovement: Int) = {
    val repeatWindow = slope.head.length
    slope.zipWithIndex.count { case (row, rowNumber) =>
      val xPosition = (xMovement * rowNumber) % repeatWindow
      row(xPosition) == '#'
    }
  }

}
