package week3.day15

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week3/day15/input.txt")
      .head
      .split(",")
      .map(_.toLong)

    utils.timeSolution("Part 1", () => solve(inputData, 2020))
    utils.timeSolution("Part 2", () => solve(inputData, 30000000))
  }

  private def solve(inputData: Array[Long], limit: Int): Long = {
    val numberRegistry = inputData.zipWithIndex.foldLeft(Map.empty[Long, List[Long]]) {
      case (registry, (number, index)) =>
        val positions = registry.getOrElse(number, List.empty[Long])
        registry + (number -> (index +: positions))
    }
    (inputData.length until limit)
      .foldLeft((numberRegistry, inputData.last)) { case ((registry, lastNumber), index) =>
        val newNumber = registry(lastNumber) match {
          case _ :: Nil                  => 0L
          case prevPos1 :: prevPos2 :: _ => prevPos1 - prevPos2
        }
        val updatedRegistry = registry.updatedWith(newNumber) {
          case None                => Some(List(index))
          case Some(prevPositions) => Some(index +: prevPositions)
        }
        (updatedRegistry, newNumber)
      }
      ._2
  }

}
