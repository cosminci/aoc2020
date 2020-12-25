package week4.day24

import cats.syntax.all._

object Solution {

  def main(args: Array[String]): Unit = {
    val directionsToTiles = utils
      .loadInputAsListOfStrings("week4/day24/input.txt")
      .map { moveDirectionsStr =>
        moveDirectionsStr.toCharArray.foldLeft((List.empty[String], Option.empty[Char])) {
          case ((completeDirections, directionBuilder), char) =>
            char match {
              case 'e' if directionBuilder.contains('s') => (completeDirections :+ "se", None)
              case 'e' if directionBuilder.contains('n') => (completeDirections :+ "ne", None)
              case 'e'                                   => (completeDirections ++ List("ne", "se"), None)
              case 'w' if directionBuilder.contains('s') => (completeDirections :+ "sw", None)
              case 'w' if directionBuilder.contains('n') => (completeDirections :+ "nw", None)
              case 'w'                                   => (completeDirections ++ List("nw", "sw"), None)
              case 's'                                   => (completeDirections, 's'.some)
              case 'n'                                   => (completeDirections, 'n'.some)
            }
        }
      }
      .map(_._1)

    utils.timeSolution("Part 1", () => solvePartOne(directionsToTiles))
    utils.timeSolution("Part 1", () => solvePartTwo(directionsToTiles))
  }

  private def solvePartOne(directionsToTiles: List[List[String]]): Int = {
    val finalTiles = getTileFlipCounts(directionsToTiles)
    finalTiles.count(_._2)
  }

  private def solvePartTwo(directionsToTiles: List[List[String]]): Int = {
    val initialTiles = getTileFlipCounts(directionsToTiles)
    (1 to 100)
      .foldLeft(initialTiles) { case (tiles, _) =>
        val blackTiles = tiles.filter(_._2)
        val blackTilesWithAllAdjacent = blackTiles.flatMap { case (tileCoordinates, true) =>
          val adjacentWhiteTiles =
            adjacentTiles(tileCoordinates).filter(t => !blackTiles.contains(t)).map(t => t -> false).toMap
          adjacentWhiteTiles + (tileCoordinates -> true)
        }
        blackTilesWithAllAdjacent.map { case (tileCoordinates, isBlack) =>
          val numAdjacentBlack = countAdjacentBlack(tileCoordinates, blackTiles)
          if (isBlack && (numAdjacentBlack == 0 || numAdjacentBlack > 2))
            (tileCoordinates, false)
          else if (!isBlack && numAdjacentBlack == 2)
            (tileCoordinates, true)
          else
            (tileCoordinates, isBlack)
        }
      }
      .count(_._2)
  }

  private def getTileFlipCounts(directionsToTiles: List[List[String]]) = {
    directionsToTiles
      .map { directionsToTile =>
        directionsToTile.foldLeft((0, 0)) {
          case ((x, y), "se") =>
            (x + 1, y)
          case ((x, y), "ne") =>
            (x, y + 1)
          case ((x, y), "sw") =>
            (x, y - 1)
          case ((x, y), "nw") =>
            (x - 1, y)
        }
      }
      .groupBy(xy => xy)
      .view
      .mapValues(_.size % 2 == 1)
      .toMap
  }

  private def countAdjacentBlack(
      tileCoordinates: (Int, Int),
      finalTiles: Map[(Int, Int), Boolean]
  ) = {
    adjacentTiles(tileCoordinates).count(tile => finalTiles.getOrElse(tile, false))
  }

  private def adjacentTiles(tileDirections: (Int, Int)) = {
    val x = tileDirections._1
    val y = tileDirections._2
    List(
      (x, y - 1),
      (x, y + 1),
      (x - 1, y),
      (x + 1, y),
      (x + 1, y + 1),
      (x - 1, y - 1)
    )
  }

}
