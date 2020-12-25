package week4.day25

import cats.syntax.all._

object Solution {

  def main(args: Array[String]): Unit = {
    val publicKey1 :: publicKey2 :: Nil = utils
      .loadInputAsListOfStrings("week4/day25/input.txt")
      .map(_.toInt)

    utils.timeSolution("Part 1", () => solvePartOne(publicKey1, publicKey2))
  }

  private def solvePartOne(publicKey1: Int, publicKey2: Int): Long = {
    val mod = 20201227
    LazyList.from(1).foldM(1) { case (acc, loop) =>
      val newAcc = (acc * 7) % mod
      if (newAcc == publicKey1) {
        (loop, publicKey2).asLeft
      } else if (newAcc == publicKey2) {
        (loop, publicKey1).asLeft
      } else {
        newAcc.asRight
      }
    } match {
      case Left((loopSize, otherKey)) =>
        (1 to loopSize).foldLeft(1L) { case (acc, _) =>
          (acc * otherKey) % mod
        }
    }
  }

}
