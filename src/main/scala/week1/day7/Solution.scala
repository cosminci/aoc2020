package week1.day7

object Solution {

  private val bagPattern      = "([a-z]+ [a-z]+) bag[s]?"
  private val parentPattern   = s"$bagPattern contain[s]? (.*)".r
  private val bagCountPattern = s"([0-9]+) $bagPattern[\\.]?".r

  def solvePartOne(bagTypes: List[Bag], myBagType: String): Int = {
    findBagsThatCanContain(bagTypes, myBagType).toSet.size
  }

  def solvePartTwo(bagTypes: List[Bag], myBagType: String): Int = {
    countBagsInBag(bagTypes, myBagType) - 1 // exclude my bag from the count
  }

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week1/day7/input.txt").map(parseInput)
    utils.timeSolution("Part 1", () => solvePartOne(inputData, "shiny gold"))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData, "shiny gold"))
  }

  private def parseInput(input: String): Bag =
    input match {
      case parentPattern(parentBag, contents) =>
        val childBags = contents.split(",").map(_.trim).collect { case bagCountPattern(count, childBag) =>
          BagCount(childBag, count.toInt)
        }
        Bag(parentBag, childBags.toList)
    }

  private def findBagsThatCanContain(bagTypes: List[Bag], myBagType: String): List[String] =
    bagTypes.collect {
      case Bag(bagType, contents) if contents.exists(_.bagType == myBagType) => bagType
    } match {
      case Nil => List.empty
      case bags =>
        bags.flatMap(findBagsThatCanContain(bagTypes, _)) ::: bags
    }

  private def countBagsInBag(bagTypes: List[Bag], bagType: String): Int = {
    bagTypes
      .find(_.bagType == bagType)
      .map(bag =>
        1 + bag.contents
          .map(bagCount => bagCount.count * countBagsInBag(bagTypes, bagCount.bagType))
          .sum
      )
      .getOrElse(0)
  }

  case class BagCount(bagType: String, count: Int)

  case class Bag(bagType: String, contents: List[BagCount])

}
