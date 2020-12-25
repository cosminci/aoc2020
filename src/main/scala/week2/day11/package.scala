package week2

import week2.day11.Solution.{SeatMap, XYDelta}

import scala.util.Try

package object day11 {

  def countOccupiedNeighbours(seatMap: SeatMap)(rowIdx: Int, colIdx: Int): Int = {
    directions.count { case XYDelta(rowDelta, colDelta) =>
      Try {
        val rowToCheck = rowIdx + rowDelta
        val colToCheck = colIdx + colDelta
        seatMap(rowToCheck)._1(colToCheck)._1 == '#'
      }.getOrElse(false)
    }
  }

  private def directions = List(
    XYDelta(-1, -1),
    XYDelta(-1, 0),
    XYDelta(-1, 1),
    XYDelta(0, -1),
    XYDelta(0, 1),
    XYDelta(1, -1),
    XYDelta(1, 0),
    XYDelta(1, 1)
  )

  def countOccupiedDirections(seatMap: SeatMap)(x: Int, y: Int): Int = {
    val yLength = seatMap(0)._1.length
    val xLength = seatMap.length

    val west = (0 until y).reverse
      .collectFirst {
        case col if seatMap(x)._1(col)._1 != '.' =>
          if (seatMap(x)._1(col)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val east = (y + 1 until yLength)
      .collectFirst {
        case col if seatMap(x)._1(col)._1 != '.' =>
          if (seatMap(x)._1(col)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val north = (0 until x).reverse
      .collectFirst {
        case row if seatMap(row)._1(y)._1 != '.' =>
          if (seatMap(row)._1(y)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val south = (x + 1 until xLength)
      .collectFirst {
        case row if seatMap(row)._1(y)._1 != '.' =>
          if (seatMap(row)._1(y)._1 == '#') 1 else 0
      }
      .getOrElse(0)

    val northwest = (1 to math.min(x, y))
      .collectFirst {
        case delta if seatMap(x - delta)._1(y - delta)._1 != '.' =>
          if (seatMap(x - delta)._1(y - delta)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val northeast = (1 to math.min(x, yLength - y - 1))
      .collectFirst {
        case delta if seatMap(x - delta)._1(y + delta)._1 != '.' =>
          if (seatMap(x - delta)._1(y + delta)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val southwest = (1 to math.min(xLength - x - 1, y))
      .collectFirst {
        case delta if seatMap(x + delta)._1(y - delta)._1 != '.' =>
          if (seatMap(x + delta)._1(y - delta)._1 == '#') 1 else 0
      }
      .getOrElse(0)
    val southeast = (1 to math.min(xLength - x - 1, yLength - y - 1))
      .collectFirst {
        case delta if seatMap(x + delta)._1(y + delta)._1 != '.' =>
          if (seatMap(x + delta)._1(y + delta)._1 == '#') 1 else 0
      }
      .getOrElse(0)

    west + east + north + south + northwest + northeast + southwest + southeast
  }

}
