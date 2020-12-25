package week1.day6

object Solution {

  def main(args: Array[String]): Unit = {
    val inputDataAcc = utils
      .loadInputAsListOfStrings("week1/day6/input.txt")
      .foldLeft(Acc(complete = List.empty, inProgress = List.empty)) { case (Acc(complete, inProgress), line) =>
        if (line == "")
          Acc(complete :+ inProgress, List.empty)
        else
          Acc(complete, inProgress :+ line)
      }
    val inputData = inputDataAcc.complete :+ inputDataAcc.inProgress

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[List[String]]): Int = {
    inputData.map(_.flatMap(_.toCharArray).toSet.size).sum
  }

  private def solvePartTwo(inputData: List[List[String]]): Int = {
    inputData.map(_.map(_.toCharArray).reduce(_ intersect _).length).sum
  }

  case class Acc(complete: List[List[String]], inProgress: List[String])

}
