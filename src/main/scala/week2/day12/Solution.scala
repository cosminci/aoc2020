package week2.day12

import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week2/day12/input.txt").map(str => (str.head, str.tail.toInt))
    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(numbers: List[(Char, Int)]): Int = {
    val finalPosition = numbers.foldLeft(ShipPosition()) { case (ShipPosition(x, y, angle), (command, amplitude)) =>
      command match {
        case 'N' => ShipPosition(x, y + amplitude, angle)
        case 'S' => ShipPosition(x, y - amplitude, angle)
        case 'W' => ShipPosition(x - amplitude, y, angle)
        case 'E' => ShipPosition(x + amplitude, y, angle)
        case 'L' =>
          val newAngle = (angle - amplitude) % 360
          if (newAngle < 0) ShipPosition(x, y, 360 + newAngle) else ShipPosition(x, y, newAngle)
        case 'R' => ShipPosition(x, y, (angle + amplitude) % 360)
        case 'F' =>
          if (angle % 360 == 0) ShipPosition(x, y + amplitude, angle)
          else if (angle % 270 == 0) ShipPosition(x - amplitude, y, angle)
          else if (angle % 180 == 0) ShipPosition(x, y - amplitude, angle)
          else ShipPosition(x + amplitude, y, angle)
      }
    }
    math.abs(finalPosition.x) + math.abs(finalPosition.y)
  }

  private def solvePartTwo(numbers: List[(Char, Int)]): Int = {
    val finalPosition = numbers.foldLeft(ShipAndWayPointPositions()) {
      case (ShipAndWayPointPositions(shipX, shipY, wayDeltaX, wayDeltaY), (command, amplitude)) =>
        command match {
          case 'N' => ShipAndWayPointPositions(shipX, shipY, wayDeltaX, wayDeltaY + amplitude)
          case 'S' => ShipAndWayPointPositions(shipX, shipY, wayDeltaX, wayDeltaY - amplitude)
          case 'W' => ShipAndWayPointPositions(shipX, shipY, wayDeltaX - amplitude, wayDeltaY)
          case 'E' => ShipAndWayPointPositions(shipX, shipY, wayDeltaX + amplitude, wayDeltaY)
          case 'L' =>
            val (x, y) = rotateLeft(wayDeltaX, wayDeltaY, amplitude / 90)
            ShipAndWayPointPositions(shipX, shipY, x, y)
          case 'R' =>
            val (x, y) = rotateRight(wayDeltaX, wayDeltaY, amplitude / 90)
            ShipAndWayPointPositions(shipX, shipY, x, y)
          case 'F' =>
            ShipAndWayPointPositions(shipX + amplitude * wayDeltaX, shipY + amplitude * wayDeltaY, wayDeltaX, wayDeltaY)
        }
    }
    math.abs(finalPosition.shipX) + math.abs(finalPosition.shipY)
  }

  @tailrec
  private def rotateLeft(x: Int, y: Int, times: Int): (Int, Int) = {
    if (times == 0) (x, y)
    else rotateLeft(-y, x, times - 1)
  }

  @tailrec
  private def rotateRight(x: Int, y: Int, times: Int): (Int, Int) = {
    if (times == 0) (x, y)
    else rotateRight(y, -x, times - 1)
  }

  case class ShipPosition(x: Int = 0, y: Int = 0, angle: Int = 90)

  case class ShipAndWayPointPositions(shipX: Int = 0, shipY: Int = 0, wayDeltaX: Int = 10, wayDeltaY: Int = 1)

}
