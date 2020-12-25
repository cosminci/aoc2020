package week4.day22

import cats.syntax.all._

import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week4/day22/input.txt").filter { str =>
      str != "" && !str.startsWith("Player")
    }
    val (player1Cards, player2Cards) = inputData.map(_.toInt).splitAt(inputData.length / 2)

    utils.timeSolution("Part 1", () => solvePartOne(player1Cards, player2Cards))
    utils.timeSolution("Part 2", () => solvePartTwo(player1Cards, player2Cards))
  }

  @tailrec
  private def playCombat(player1Cards: List[Int], player2Cards: List[Int]): List[Int] =
    (player1Cards, player2Cards) match {
      case (Nil, player2WinningStack) => player2WinningStack
      case (player1WinningStack, Nil) => player1WinningStack
      case (player1TopCard :: player1Remaining, player2TopCard :: player2Remaining) =>
        if (player1TopCard > player2TopCard) {
          playCombat(player1Remaining ++ List(player1TopCard, player2TopCard), player2Remaining)
        } else {
          playCombat(player1Remaining, player2Remaining ++ List(player2TopCard, player1TopCard))
        }
    }

  private def solvePartOne(player1Cards: List[Int], player2Cards: List[Int]): Int = {
    playCombat(player1Cards, player2Cards).reverse.zipWithIndex.foldLeft(0) { case (acc, (card, index)) =>
      acc + card * (index + 1)
    }
  }

  private def playRecursiveCombat(
      player1Cards: List[Int],
      player2Cards: List[Int],
      previousRounds: Set[(List[Int], List[Int])]
  ): Either[List[Int], List[Int]] = {
    (player1Cards, player2Cards) match {
      case (Nil, player2WinningStack) =>
        player2WinningStack.asRight

      case (player1WinningStack, Nil) =>
        player1WinningStack.asLeft

      case (player1TopCard :: player1Remaining, player2TopCard :: player2Remaining) =>
        if (previousRounds.contains((player1Cards, player2Cards))) {
          player1Cards.asLeft
        } else {
          val player1Wins = if (player1Remaining.length < player1TopCard || player2Remaining.length < player2TopCard) {
            player1TopCard > player2TopCard
          } else {
            playRecursiveCombat(
              player1Remaining.take(player1TopCard),
              player2Remaining.take(player2TopCard),
              Set.empty
            ).isLeft
          }

          val (newPlayer1Cards, newPlayer2Cards) = if (player1Wins) {
            (player1Remaining ++ List(player1TopCard, player2TopCard), player2Remaining)
          } else {
            (player1Remaining, player2Remaining ++ List(player2TopCard, player1TopCard))
          }

          playRecursiveCombat(newPlayer1Cards, newPlayer2Cards, previousRounds + ((player1Cards, player2Cards)))
        }
    }
  }

  private def solvePartTwo(player1Cards: List[Int], player2Cards: List[Int]): Long = {
    val winningCardStack = playRecursiveCombat(player1Cards, player2Cards, Set.empty) match {
      case Left(player1Cards)  => player1Cards
      case Right(player2Cards) => player2Cards
    }
    winningCardStack.reverse.zipWithIndex.foldLeft(0L) { case (acc, (card, index)) =>
      acc + card * (index + 1)
    }
  }

}
