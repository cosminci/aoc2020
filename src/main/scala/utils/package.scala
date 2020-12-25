import scala.io.Source
import scala.util.Using

package object utils {

  def timeSolution[R](opName: String, op: () => R): Unit = {
    val time     = System.currentTimeMillis()
    val result   = op()
    val duration = System.currentTimeMillis() - time
    println(s"$opName result: $result. Took ${duration}ms to solve.")
  }

  def loadInputAsListOfIntegers(path: String): List[Int] =
    loadInputAsListOfStrings(path).map(_.toInt)

  def loadInputAsListOfLongs(path: String): List[Long] =
    loadInputAsListOfStrings(path).map(_.toLong)

  def loadInputAsListOfStrings(path: String): List[String] =
    Using.resource(Source.fromResource(path))(r => r.getLines().toList)

  def cartesianProductFromList[T](list: List[T], max: T)(implicit ord: Ordering[T]): Set[(T, T)] = {
    val filtered = list.filter(ord.lt(_, max))
    filtered
      .foldLeft(Set.empty[(T, T)]) { case (pairs, n1) =>
        pairs ++ filtered.collect {
          case n2 if n2 != n1 =>
            if (ord.gt(n2, n1)) (n1, n2) else (n2, n1)
        }
      }
  }

  def flipVertical[T](tile: List[List[T]]): List[List[T]]   = tile.reverse

  def rotateClockwise[T](m: List[List[T]], times: Int = 1): List[List[T]] = {
    (1 to times).foldLeft(m) { case (m, _) =>
      flipHorizontal(transpose(m))
    }
  }

  def flipHorizontal[T](tile: List[List[T]]): List[List[T]] = tile.map(_.reverse)

  def transpose[T](m: List[List[T]]): List[List[T]] = {
    m.head.indices.map { c =>
      m.indices.map { r =>
        m(r)(c)
      }.toList
    }.toList
  }

}
