package week3.day16

import scala.annotation.tailrec

object Solution {

  type Ticket = Array[Int]
  private val rulePattern = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week3/day16/input.txt")

    val rules   = inputData.takeWhile(_ != "").map(parseRule)
    val tickets = inputData.splitAt(rules.length + 2)._2
    val (myTicket, otherTickets) = tickets.splitAt(2) match {
      case (ticket, tickets) => (parseTicket(ticket.head), tickets.tail.map(parseTicket))
    }

    utils.timeSolution("Part 1", () => solvePartOne(rules, otherTickets))
    utils.timeSolution("Part 2", () => solvePartTwo(rules, myTicket, otherTickets))
  }

  @tailrec
  private def mergeIntervals(intervals: List[Interval], acc: List[Interval]): List[Interval] = intervals match {
    case a :: b :: rest if a.max >= b.min => mergeIntervals(Interval(a.min, math.max(a.max, b.max)) :: rest, acc)
    case a :: b :: rest                   => mergeIntervals(b :: rest, acc :+ a)
    case a :: Nil                         => acc :+ a
    case _                                => acc
  }

  private def solvePartOne(rules: List[Rule], otherTickets: List[Ticket]): Int = {
    val mergedIntervals = mergeIntervals(rules.flatMap(r => List(r.intervalA, r.intervalB)).sortBy(_.min), List.empty)
    otherTickets.flatMap(t => t.filter(v => mergedIntervals.forall(i => v < i.min || v > i.max))).sum
  }

  private def parseTicket(ticket: String) = ticket.split(",").map(_.toInt)

  private def parseRule(rule: String): Rule = rule match {
    case rulePattern(field, min1, max1, min2, max2) =>
      Rule(field, Interval(min1.toInt, max1.toInt), Interval(min2.toInt, max2.toInt))
  }

  private def isValid(rule: Rule, value: Int): Boolean = {
    (value >= rule.intervalA.min &&
      value <= rule.intervalA.max) ||
    (value >= rule.intervalB.min &&
      value <= rule.intervalB.max)
  }

  private def solvePartTwo(rules: List[Rule], myTicket: Ticket, tickets: List[Ticket]): Long = {
    val mergedIntervals = mergeIntervals(rules.flatMap(r => List(r.intervalA, r.intervalB)).sortBy(_.min), List.empty)
    val validTickets    = tickets.filter(t => t.forall(v => mergedIntervals.exists(i => v >= i.min && v <= i.max)))

    val assignableIndices = rules.map { rule =>
      (
        rule,
        myTicket.indices.filter { index =>
          validTickets.forall { ticket =>
            isValid(rule, ticket(index))
          }
        }
      )
    }
    val sortedAssignableIndices = assignableIndices.sortBy { case (_, indices) => indices.size }
    val assignedIndices = sortedAssignableIndices.foldLeft(List.empty[(Rule, Int)]) {
      case (alreadyAssigned, (rule, assignableIndices)) =>
        alreadyAssigned ++ assignableIndices.collectFirst {
          case index if !alreadyAssigned.exists(_._2 == index) => (rule, index)
        }
    }
    assignedIndices.collect {
      case (rule, index) if rule.field.startsWith("departure") =>
        myTicket(index).toLong
    }.product
  }

  case class Rule(field: String, intervalA: Interval, intervalB: Interval)

  case class Interval(min: Int, max: Int)

}
