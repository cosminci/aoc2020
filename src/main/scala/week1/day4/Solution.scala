package week1.day4

import scala.util.matching.Regex

object Solution {

  private val requiredFields = Map(
    ("byr", WithBounds("^([0-9]{4})$".r, min = 1920, max = 2002)),
    ("iyr", WithBounds("^([0-9]{4})$".r, min = 2010, max = 2020)),
    ("eyr", WithBounds("^([0-9]{4})$".r, min = 2020, max = 2030)),
    (
      "hgt",
      Either(WithBounds("^([0-9]+)cm$".r, min = 150, max = 193), WithBounds("^([0-9]+)in$".r, min = 59, max = 76))
    ),
    ("hcl", Simple("^#[0-9a-f]{6}$".r)),
    ("ecl", Simple("^(amb|blu|brn|gry|grn|hzl|oth)$".r)),
    ("pid", Simple("^[0-9]{9}$".r))
  )

  def main(args: Array[String]): Unit = {
    val inputDataAcc = utils
      .loadInputAsListOfStrings("week1/day4/input.txt")
      .foldLeft(Acc(complete = List.empty, inProgress = "")) { case (Acc(complete, inProgress), line) =>
        if (line == "")
          Acc(complete :+ inProgress, "")
        else
          Acc(complete, s"$inProgress $line")
      }
    val inputData = inputDataAcc.complete :+ inputDataAcc.inProgress

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[String]): Int = {
    inputData.count { inputLine =>
      val passportFields = inputLine.trim.split(" ").map(_.split(":").head).toSet
      requiredFields.keys.forall(passportFields.contains)
    }
  }

  private def solvePartTwo(inputData: List[String]): Int = {
    inputData.count { passport =>
      val existingFields = passport.trim.split(" ").map(_.split(":").toList).collect { case key :: value :: Nil =>
        (key, value)
      }
      requiredFields.forall { case (fieldName, rule) =>
        existingFields.find(_._1 == fieldName).exists(existing => isValid(existing._2, rule))
      }
    }
  }

  private def isValid(value: String, rule: Rule): Boolean = rule match {
    case Simple(regex) =>
      regex.matches(value)
    case WithBounds(regex, min, max) =>
      value match {
        case regex(matched) =>
          val v = matched.toInt
          v >= min && v <= max
        case _ => false
      }
    case Either(rule1, rule2) =>
      isValid(value, rule1) || isValid(value, rule2)
  }

  sealed trait Rule

  case class Acc(complete: List[String], inProgress: String)

  case class Simple(regex: Regex) extends Rule

  case class WithBounds(regex: Regex, min: Int, max: Int) extends Rule

  case class Either(rule1: Rule, rule2: Rule) extends Rule

}
