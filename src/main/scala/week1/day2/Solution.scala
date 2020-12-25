package week1.day2

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week1/day2/input_ex.txt").map(parseInput)

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def parseInput(str: String) = {
    str.split(" ").toList match {
      case range :: charStr :: pw :: Nil =>
        val r = range.split("-").map(_.toInt)
        Input(r(0), r(1), charStr.charAt(0), pw)
    }
  }

  private def solvePartOne(inputData: List[Input]): Int = {
    inputData.count { case Input(minCount, maxCount, char, pw) =>
      val count = pw.count(_ == char)
      count >= minCount && count <= maxCount
    }
  }

  private def solvePartTwo(inputData: List[Input]): Int = {
    inputData.count { case Input(position1, position2, char, pw) =>
      (pw.charAt(position1 - 1) == char) ^ (pw.charAt(position2 - 1) == char)
    }
  }

  case class Input(n1: Int, n2: Int, char: Char, pw: String)
}
