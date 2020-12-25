package week3.day19

object Solution {

  private val ruleIdPattern          = "([0-9]+): (.*)$".r
  private val ruleLetterPattern      = "\"([a-z])\"".r
  private val ruleCompositionPattern = "(.*) \\| (.*)".r

  def main(args: Array[String]): Unit = {
    val inputData = utils
      .loadInputAsListOfStrings("week3/day19/input.txt")

    val rules = inputData
      .takeWhile(_ != "")
      .map { case ruleIdPattern(idStr, rest) =>
        val id = idStr.toInt
        rest match {
          case ruleLetterPattern(letter) =>
            id -> LetterRule(letter.charAt(0))
          case ruleCompositionPattern(rules1, rules2) =>
            id -> CompositeRule(ruleIdList(rules1), ruleIdList(rules2))
          case _ =>
            id -> RuleSequence(ruleIdList(rest))
        }
      }
      .toMap

    val toValidate = inputData.splitAt(rules.size + 1)._2

    utils.timeSolution("Part 1", () => solvePartOne(rules, toValidate))
    utils.timeSolution("Part 2", () => solvePartTwo(rules, toValidate))
  }

  private def ruleIdList(str: String): List[Int] = str.trim.split(" ").map(_.toInt).toList

  private def validMatches(rules: Map[Int, Rule], rule: Rule, str: String): List[Matched] = {
    if (str == "") {
      List.empty
    } else {
      rule match {
        case LetterRule(letter) =>
          List.empty ++ Option.when(str.charAt(0) == letter)(Matched(str.tail))
        case RuleSequence(ruleIds) =>
          matchesSequence(rules, ruleIds, str)
        case CompositeRule(rules1, rules2) =>
          val firstMatchResults  = matchesSequence(rules, rules1, str)
          val secondMatchResults = matchesSequence(rules, rules2, str)
          firstMatchResults ++ secondMatchResults
      }
    }
  }

  private def matchesSequence(rules: Map[Int, Rule], ruleIds: List[Int], str: String): List[Matched] = {
    val accInit = validMatches(rules, rules(ruleIds.head), str)
    ruleIds.tail.foldLeft(accInit) { (matchAcc, ruleId) =>
      matchAcc.flatMap { prevMatch =>
        validMatches(rules, rules(ruleId), prevMatch.remainingStr)
      }
    }
  }

  private def solvePartOne(rules: Map[Int, Rule], toValidate: List[String]): Int = {
    val rootRule = rules(0)
    toValidate.count { str =>
      validMatches(rules, rootRule, str).exists(_.remainingStr == "")
    }
  }

  private def solvePartTwo(rules: Map[Int, Rule], toValidate: List[String]): Int = {
    val cyclicRules = rules ++ Map(
      8  -> CompositeRule(List(42, 8), List(42)),
      11 -> CompositeRule(List(42, 11, 31), List(42, 31))
    )
    solvePartOne(cyclicRules, toValidate)
  }

  sealed trait Rule

  case class LetterRule(letter: Char) extends Rule

  case class CompositeRule(rules1: List[Int], rules2: List[Int]) extends Rule

  case class RuleSequence(rules: List[Int]) extends Rule

  case class Matched(remainingStr: String)

}
