package week1.day1

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfIntegers("week1/day1/input.txt")
    val targetSum = 2020

    utils.timeSolution("Part 1", () => solvePartOne(inputData, targetSum))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData, targetSum))
  }

  private def solvePartOne(input: List[Int], targetSum: Int) = {
    input.find(x => input.exists(y => x + y == targetSum)).map(x => x * (targetSum - x))
  }

  private def solvePartTwo(input: List[Int], targetSum: Int) = {
    utils
      .cartesianProductFromList(input, targetSum)
      .collectFirst {
        case (x, y) if x + y < targetSum && input.exists(z => x + y + z == targetSum) =>
          x * y * (targetSum - (x + y))
      }
  }

}
