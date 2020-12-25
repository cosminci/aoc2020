package week2.day8

import week2.day8.Solution.ArgSign.ArgSign
import week2.day8.Solution.OpType.OpType

import scala.annotation.tailrec

object Solution {

  private val opPattern = "([a-z]+) ([+|-])([0-9]+)".r

  @tailrec
  def execute(acc: Int, opToExecute: Int, executedOps: Set[Int], indexedOps: Vector[Op]): (Int, Set[Int], Int) = {
    if (executedOps.contains(opToExecute) || !indexedOps.isDefinedAt(opToExecute))
      (acc, executedOps, opToExecute)
    else {
      val newExecutedOps = executedOps + opToExecute
      indexedOps(opToExecute) match {
        case Op(OpType.nop, _, _) =>
          execute(acc, opToExecute + 1, newExecutedOps, indexedOps)
        case Op(OpType.acc, argSign, argValue) =>
          val newAcc = if (argSign == ArgSign.plus) acc + argValue else acc - argValue
          execute(newAcc, opToExecute + 1, newExecutedOps, indexedOps)
        case Op(OpType.jmp, argSign, argValue) =>
          val newOp = if (argSign == ArgSign.plus) opToExecute + argValue else opToExecute - argValue
          execute(acc, newOp, newExecutedOps, indexedOps)
      }
    }
  }

  def solvePartTwo(ops: Vector[Op]): Option[Int] = {
    val (_, executedOps, _) = execute(acc = 0, opToExecute = 0, executedOps = Set.empty, ops)
    val candidateOps = executedOps.collect {
      case opIdx if List(OpType.nop, OpType.jmp).contains(ops(opIdx).kind) =>
        val Op(kind, sign, value) = ops(opIdx)
        val newKind               = if (kind == OpType.nop) OpType.jmp else OpType.nop
        (opIdx, Op(newKind, sign, value))
    }
    candidateOps.collectFirst {
      case op if replaceAndExecute(ops, op)._3 == ops.length =>
        replaceAndExecute(ops, op)._1
    }
  }

  def solvePartOne(ops: Vector[Op]): Int =
    execute(acc = 0, opToExecute = 0, executedOps = Set.empty, ops)._1

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week2/day8/input.txt").map(parseInput).toVector
    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def parseInput(input: String): Op =
    input match {
      case opPattern(opType, argSign, argValue) =>
        val sign = if (argSign == "+") ArgSign.plus else ArgSign.minus
        Op(OpType.withName(opType), sign, argValue.toInt)
    }

  private def replaceAndExecute(ops: Vector[Op], op: (Int, Op)) = {
    execute(
      acc = 0,
      opToExecute = 0,
      executedOps = Set.empty,
      ops.updated(op._1, op._2)
    )
  }

  case class Op(kind: OpType, argSign: ArgSign, argValue: Int)

  case class Acc(executedLines: Set[Int] = Set.empty, opAcc: Int = 0)

  object OpType extends Enumeration {
    type OpType = Value
    val nop, jmp, acc = Value
  }

  object ArgSign extends Enumeration {
    type ArgSign = Value
    val plus, minus = Value
  }

}
