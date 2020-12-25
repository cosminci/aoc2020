package week3.day17

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week3/day17/input.txt")
      .map(_.toCharArray.filter(_ != ','))
      .toArray

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def neighboursInclSelf[T <: Point](point: T): Set[T] = {
    point match {
      case p: Point3D =>
        neighboursInclSelf3d(p).asInstanceOf[Set[T]]
      case p: Point4D =>
        neighboursInclSelf4d(p).asInstanceOf[Set[T]]
    }
  }

  private def neighboursInclSelf3d(point: Point3D): Set[Point3D] = {
    val samePlane = Set(
      point,
      point.copy(x = point.x - 1),
      point.copy(x = point.x + 1),
      point.copy(y = point.y - 1),
      point.copy(y = point.y + 1),
      point.copy(x = point.x - 1, y = point.y - 1),
      point.copy(x = point.x - 1, y = point.y + 1),
      point.copy(x = point.x + 1, y = point.y - 1),
      point.copy(x = point.x + 1, y = point.y + 1)
    )
    val upperPlane = samePlane.map(p => p.copy(z = p.z + 1))
    val lowerPlane = samePlane.map(p => p.copy(z = p.z - 1))
    samePlane ++ upperPlane ++ lowerPlane
  }

  private def neighboursInclSelf4d(point: Point4D): Set[Point4D] = {
    neighboursInclSelf3d(Point3D(point.x, point.y, point.z)).flatMap { point3d =>
      Set(point.w - 1, point.w, point.w + 1).map(coord => Point4D(point3d.x, point3d.y, point3d.z, coord))
    }
  }

  private def generatePoints[T <: Point](existingPoints: Set[T]): Set[T] = {
    existingPoints
      .foldLeft((Set.empty[T], Set.empty[T])) { case ((newPoints, exploredPoints), existingPoint) =>
        val toExplore = neighboursInclSelf(existingPoint)
        (
          newPoints ++ toExplore.diff(exploredPoints).flatMap { point =>
            val activeNeighbourCount = (neighboursInclSelf(point) - point).count(existingPoints.contains)
            if (existingPoints.contains(point)) {
              if (activeNeighbourCount == 2 || activeNeighbourCount == 3)
                Some(point)
              else
                None
            } else {
              if (activeNeighbourCount == 3)
                Some(point)
              else None
            }
          },
          exploredPoints ++ toExplore
        )
      }
      ._1
  }

  private def solvePartOne(inputData: Array[Array[Char]]): Int = {
    val initialPoints = inputData.indices.flatMap { row =>
      inputData(row).indices.collect {
        case col if inputData(row)(col) == '#' =>
          Point3D(row, col, 0)
      }
    }.toSet

    (0 until 6)
      .foldLeft(initialPoints) { case (existingPoints, _) =>
        val points = generatePoints(existingPoints)
        points
      }
      .size
  }

  private def solvePartTwo(inputData: Array[Array[Char]]): Int = {
    val initialPoints = inputData.indices.flatMap { row =>
      inputData(row).indices.collect {
        case col if inputData(row)(col) == '#' =>
          Point4D(row, col, 0, 0)
      }
    }.toSet

    (0 until 6)
      .foldLeft(initialPoints) { case (existingPoints, _) =>
        val points = generatePoints(existingPoints)
        points
      }
      .size
  }

  sealed trait Point

  case class Point3D(x: Int, y: Int, z: Int)         extends Point

  case class Point4D(x: Int, y: Int, z: Int, w: Int) extends Point

}
