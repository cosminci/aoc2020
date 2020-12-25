package week2.day13

object Solution {

  implicit val departureOrdering: Ordering[BusDepartureDelta] = (x: BusDepartureDelta, y: BusDepartureDelta) =>
    x.delta.compareTo(y.delta)

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week2/day13/input.txt")

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[String]): Int = {
    val minimumTs = inputData.head.toInt
    val buses     = inputData.last.split(",").filter(_ != "x").map(_.toInt)
    val busDeparture = buses.map { busId =>
      val departureDelta = (minimumTs / busId + 1) * busId - minimumTs
      BusDepartureDelta(busId, departureDelta)
    }.min
    busDeparture.busId * busDeparture.delta
  }

  private def solvePartTwo(inputData: List[String]): Long = {
    crt(inputData.last.split(",").zipWithIndex.filter(_._1 != "x").toList.map { case (busNo, offset) =>
      val bus = busNo.toLong
      (bus, (bus - offset) % bus)
    })
  }

  case class BusDepartureDelta(busId: Int, delta: Int)

}
