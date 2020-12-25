package week3.day18

object Solution {

  private val numberPattern = "([0-9]+)".r

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week3/day18/input_ex.txt")

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[String]): Long = {
    inputData.map(evaluateExpressionPartOne).sum
  }

  private def evaluateExpressionPartOne(expr: String): Long = {
    expr
      .replace(" ", "")
      .toCharArray
      .foldLeft(List.empty[Elem]) { case (acc, el) =>
        el match {
          case numberPattern(value) =>
            val currentOperand = Operand(value.asDigit)
            acc.lastOption match {
              case Some(operation: Operation) =>
                acc.splitAt(acc.length - 2) match {
                  case (remainder, (prevOperand: Operand) :: _ :: Nil) =>
                    remainder :+ eval(prevOperand, currentOperand, operation)
                }
              case _ =>
                acc :+ currentOperand
            }
          case o if o == '+' || o == '*' =>
            acc :+ Operation(o)
          case '(' =>
            acc :+ LeftParan
          case ')' =>
            val (remainder, _ :: (operand: Operand) :: Nil) = acc.splitAt(acc.length - 2)
            remainder.reverse match {
              case Nil =>
                List(operand)
              case LeftParan :: _ =>
                remainder :+ operand
              case (operation: Operation) :: (prevOperand: Operand) :: rem =>
                rem.reverse :+ eval(prevOperand, operand, operation)
            }
        }
      }
      .head
      .asInstanceOf[Operand]
      .v
  }

  def eval(op1: Operand, op2: Operand, operation: Operation): Operand = operation.v match {
    case '+' => Operand(op1.v + op2.v)
    case '*' => Operand(op1.v * op2.v)
  }

  private def solvePartTwo(inputData: List[String]): Long = {
    inputData.map(evaluateExpressionPartTwo).sum
  }

  private def evaluateExpressionPartTwo(expr: String): Long = {
    val value1 = expr
      .replace(" ", "")
      .toCharArray
      .foldLeft(List.empty[Elem]) { case (acc, el) =>
        el match {
          case numberPattern(value) =>
            val currentOperand = Operand(value.asDigit)
            acc.lastOption match {
              case Some(operation @ Operation('+')) =>
                acc.splitAt(acc.length - 2) match {
                  case (remainder, (prevOperand: Operand) :: _ :: Nil) =>
                    remainder :+ eval(prevOperand, currentOperand, operation)
                }
              case _ =>
                acc :+ currentOperand
            }
          case o if o == '+' || o == '*' =>
            acc :+ Operation(o)
          case '(' =>
            acc :+ LeftParan
          case ')' =>
            evaluatePrevious(acc)
        }
      }
    value1.collect { case Operand(v) =>
      v
    }.product
  }

  def evaluatePrevious(acc: List[Elem]): List[Elem] = {
    val toMultiply = acc.reverse.takeWhile(_ != LeftParan)
    val newOperand = Operand(toMultiply.collect { case Operand(v) =>
      v
    }.product)
    val remainder = acc.splitAt(acc.length - toMultiply.length - 1)._1

    remainder.reverse match {
      case (operation @ Operation('+')) :: (prevOperand: Operand) :: rem =>
        rem.reverse :+ eval(prevOperand, newOperand, operation)
      case _ =>
        remainder :+ newOperand
    }
  }

  sealed trait Elem

  case class Operand(v: Long)   extends Elem

  case class Operation(v: Char) extends Elem

  case object LeftParan         extends Elem

}
