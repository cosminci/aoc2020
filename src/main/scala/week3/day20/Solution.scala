package week3.day20

import utils._

object Solution {

  type FindMatchingTileFn = (List[Char], Int) => TileMatch
  private val tileIdPattern = "Tile ([0-9]+):$".r

  def main(args: Array[String]): Unit = {
    val tiles   = loadTiles()
    val monster = loadMonster()

    utils.timeSolution("Part 1", () => solvePartOne(tiles))
    utils.timeSolution("Part 2", () => solvePartTwo(tiles, monster))
  }

  private def tileEdges(data: List[List[Char]]) = {
    List(data.head, data.map(_.last), data.last.reverse, data.map(_.head).reverse)
  }

  private def findEdgePairs(tiles: List[Tile]) = {
    val edgesWithTileIds = tiles.collect { case Tile(tileId, data) =>
      tileEdges(data).map(_ -> tileId)
    }.flatten

    edgesWithTileIds.map { case (edge, tileId) =>
      val reversedEdge = edge.reverse
      val matchingTiles = edgesWithTileIds.collect {
        case (e, id) if id != tileId && (reversedEdge == e || edge == e) => id
      }
      edge -> (matchingTiles :+ tileId)
    }.toMap
  }

  private def findCornerTiles(edgeMatches: Map[List[Char], List[Int]]) = {
    val edgeTiles = edgeMatches.collect {
      case (_, tileIds) if tileIds.size == 1 => tileIds.head
    }.toList
    edgeTiles.diff(edgeTiles.toSet.toList)
  }

  private def solvePartOne(tiles: List[Tile]): Long = {
    val edgeMatches = findEdgePairs(tiles)
    val cornerTiles = findCornerTiles(edgeMatches)

    cornerTiles.map(_.toLong).product
  }

  private def stitchRow(tile: Tile, findMatchingTile: FindMatchingTileFn, times: Int) = {
    (1 until times)
      .foldLeft((tile.id, tile.rows)) { case ((lastStitchedTile, rowBuilder), _) =>
        val rightEdge    = rowBuilder.map(_.last)
        val tileMatch    = findMatchingTile(rightEdge, lastStitchedTile)
        val edgeMatch    = findMatchingEdge(rightEdge, tileMatch)
        val tileToStitch = alignHorizontalTileToStitch(tileMatch, edgeMatch)
        val stitched = rowBuilder.zip(tileToStitch).map { case (alreadyStitched, toStitch) =>
          alreadyStitched ::: toStitch
        }
        (tileMatch.tile.id, stitched)
      }
      ._2
  }

  private def buildMap(tiles: List[Tile]) = {
    val mapSizeX, mapSizeY = math.sqrt(tiles.size).toInt
    val tileRegistry       = tiles.map { case Tile(id, rows) => id -> rows }.toMap
    val edgeMatches        = findEdgePairs(tiles)

    def findMatchingTile = findMatchingTile_(tileRegistry, edgeMatches) _

    // TODO: choose and align top left corner programmatically
    // for input_ex.txt
//    val rootUpperLeftCorner = Tile(1951, flipVertical(tileRegistry(1951)))
    // for input.txt
    val rootUpperLeftCorner = Tile(1811, rotateClockwise(tileRegistry(1811), times = 2))

    val stitchedRow = stitchRow(rootUpperLeftCorner, findMatchingTile, mapSizeX)
    val stitchedMap = (1 until mapSizeY)
      .foldLeft((rootUpperLeftCorner, stitchedRow)) { case ((prevUpperLeftCorner, mapBuilder), _) =>
        val edges              = tileEdges(prevUpperLeftCorner.rows)
        val lowerEdge          = edges(2)
        val tileMatch          = findMatchingTile(lowerEdge, prevUpperLeftCorner.id)
        val edgeMatch          = findMatchingEdge(lowerEdge, tileMatch)
        val newUpperLeftCorner = Tile(tileMatch.tile.id, alignVerticalTileToStitch(tileMatch, edgeMatch))
        val newRow             = stitchRow(newUpperLeftCorner, findMatchingTile, mapSizeX)
        (newUpperLeftCorner, mapBuilder ::: newRow)
      }
      ._2

    removeBorders(stitchedMap, tiles.head.rows.size)
  }

  private def removeBorders(stitchedMap: List[List[Char]], tileSize: Int) = {
    stitchedMap.grouped(tileSize).flatMap { rowGroup =>
      rowGroup.slice(1, tileSize - 1).map { row =>
        row.grouped(tileSize).flatMap { r =>
          r.slice(1, tileSize - 1)
        }
      }
    }
  }

  private def solvePartTwo(tiles: List[Tile], monster: Monster): Long = {
    val stitchedMap = buildMap(tiles).map(_.toList).toList
    val (map, monsterPositions) =
      (0 to 3)
        .map(n => rotateClockwise(stitchedMap, n))
        .flatMap(map => List(map, flipHorizontal(map)))
        .map { map =>
          val monsterPositions = (0 until map.length - monster.height).foldLeft(List.empty[(Int, Int)]) {
            case (monsterStartPositions, rowNumber) =>
              monsterStartPositions ++ (0 until map.head.length - monster.length).collect {
                case colNumber if monsterPresent(map, rowNumber, colNumber, monster) =>
                  (rowNumber, colNumber)
              }
          }
          map -> monsterPositions
        }
        .find { case (_, monsterPositions) =>
          monsterPositions.nonEmpty
        }
        .get

    map.indices.foldLeft(0) { case (acc, rowNumber) =>
      acc + map(rowNumber).indices.count { colNumber =>
        map(rowNumber)(colNumber) == '#'
      }
    } - (monsterPositions.length * monster.parts.length)
  }

  private def monsterPresent(
      map: List[List[Char]],
      rowNumber: Int,
      colNumber: Int,
      monster: Monster
  ) = {
    monster.parts.forall { case (x, y) =>
      map(rowNumber + x)(colNumber + y) == '#'
    }
  }

  private def findMatchingEdge(edge: List[Char], tileMatch: TileMatch) = {
    tileEdges(tileMatch.tile.rows).zipWithIndex.collectFirst {
      case (row, idx) if row == edge =>
        EdgeMatch(reversed = false, idx)
      case (row, idx) if row.reverse == edge =>
        EdgeMatch(reversed = true, idx)
    }.get
  }

  private def findMatchingTile_(
      tileRegistry: Map[Int, List[List[Char]]],
      edgeMatches: Map[List[Char], List[Int]]
  )(edge: List[Char], ownerTile: Int) = {
    val matchedStraight = edgeMatches.get(edge).flatMap { tileIds =>
      tileIds.find(_ != ownerTile).map(id => TileMatch(reversed = false, Tile(id, tileRegistry(id))))
    }
    val matchedReversed = edgeMatches.get(edge.reverse).flatMap { tileIds =>
      tileIds.find(_ != ownerTile).map(id => TileMatch(reversed = true, Tile(id, tileRegistry(id))))
    }
    matchedStraight.getOrElse(matchedReversed.get)
  }

  private def alignHorizontalTileToStitch(tileMatch: TileMatch, edgeMatch: EdgeMatch) = {
    if (edgeMatch.reversed) {
      rotateClockwise(tileMatch.tile.rows, 3 - edgeMatch.index)
    } else {
      if (edgeMatch.index == 0) {
        rotateClockwise(flipVertical(tileMatch.tile.rows))
      } else if (edgeMatch.index == 1) {
        flipHorizontal(tileMatch.tile.rows)
      } else if (edgeMatch.index == 2) {
        flipVertical(rotateClockwise(tileMatch.tile.rows))
      } else {
        flipVertical(tileMatch.tile.rows)
      }
    }
  }

  private def alignVerticalTileToStitch(tileMatch: TileMatch, edgeMatch: EdgeMatch) = {
    if (edgeMatch.reversed) {
      rotateClockwise(tileMatch.tile.rows, (4 - edgeMatch.index) % 4)
    } else {
      if (edgeMatch.index == 0) {
        flipHorizontal(tileMatch.tile.rows)
      } else if (edgeMatch.index == 1) {
        rotateClockwise(flipHorizontal(tileMatch.tile.rows))
      } else if (edgeMatch.index == 2) {
        flipVertical(tileMatch.tile.rows)
      } else {
        rotateClockwise(flipVertical(tileMatch.tile.rows))
      }
    }
  }

  private def loadMonster() = {
    val monsterData = utils
      .loadInputAsListOfStrings("week3/day20/monster.txt")
      .map(_.toCharArray)

    val monsterLength = monsterData.map(_.length).max
    val monsterHeight = monsterData.length

    val monsterParts = monsterData.indices.foldLeft(List.empty[(Int, Int)]) { case (positions, rowNumber) =>
      val row = monsterData(rowNumber)
      positions ++ row.indices.collect {
        case colNumber if row(colNumber) == '#' => (rowNumber, colNumber)
      }
    }

    Monster(monsterParts, monsterLength, monsterHeight)
  }

  private def loadTiles() = {
    val inputData = utils.loadInputAsListOfStrings("week3/day20/input.txt")
    val tileSize  = inputData.tail.head.length

    inputData
      .grouped(tileSize + 2)
      .map { case tileIdPattern(tileId) :: tileData =>
        val data = tileData.take(tileSize).map(_.toCharArray.toList)
        Tile(tileId.toInt, data)
      }
      .toList
  }

  case class Tile(id: Int, rows: List[List[Char]])

  case class Monster(parts: List[(Int, Int)], length: Int, height: Int)

  case class TileMatch(reversed: Boolean, tile: Tile)

  case class EdgeMatch(reversed: Boolean, index: Int)
}
