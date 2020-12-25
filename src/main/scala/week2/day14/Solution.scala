package week2.day14

object Solution {

  private val maskPattern   = "mask = ([01X]+)?".r
  private val memoryPattern = "mem\\[([0-9]+)] = ([0-9]+)?".r

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsListOfStrings("week2/day14/input.txt")

    utils.timeSolution("Part 1", () => solvePartOne(inputData))
    utils.timeSolution("Part 2", () => solvePartTwo(inputData))
  }

  private def solvePartOne(inputData: List[String]): Long = {
    val commands = inputData.map {
      case maskPattern(maskStr) =>
        Mask(maskStr.toCharArray.reverse.zipWithIndex.filter(_._1 != 'X').map(_.swap).toMap)
      case memoryPattern(locationStr, valueStr) =>
        Store(locationStr.toInt, valueStr.toLong)
    }
    applyCommands(commands)
  }

  private def applyCommands(commands: List[Command]) = {
    val initialMask = Mask(Map.empty[Int, Char])
    commands
      .foldLeft((Map.empty[Int, Long], initialMask)) { case ((memory, mask), command) =>
        command match {
          case m @ Mask(_) =>
            (memory, m)
          case Store(location, value) =>
            (memory + (location -> maskValue(mask.bits, value)), mask)
        }
      }
      ._1
      .collect {
        case (_, value) if value != 0 => value
      }
      .sum
  }

  private def maskValue(bitsToChange: Map[Int, Char], value: Long): Long = {
    bitArrayToLong(applyMask(bitsToChange, value))
  }

  private def bitArrayToLong(arr: Array[Char]): Long = BigInt(arr.mkString(""), 2).toLong

  private def applyMask(bitsToChange: Map[Int, Char], value: Long) = {
    val binaryRepr = bitsToChange
      .foldLeft(value.toBinaryString.toCharArray.reverse) { case (acc, (idx, char)) =>
        val bitArray = acc.padTo(math.max(acc.length, idx + 1), '0')
        bitArray(idx) = char
        bitArray
      }
      .reverse
    binaryRepr
  }

  private def solvePartTwo(inputData: List[String]): Long = {
    val initialMask = Mask(Map.empty[Int, Char])
    val commands = inputData.map {
      case maskPattern(maskStr) =>
        Mask(maskStr.toCharArray.reverse.zipWithIndex.filter(_._1 != '0').map(_.swap).toMap)
      case memoryPattern(locationStr, valueStr) =>
        Store(locationStr.toInt, valueStr.toLong)
    }

    commands
      .foldLeft((Map.empty[Long, Long], initialMask)) { case ((memory, mask), command) =>
        command match {
          case m @ Mask(_) =>
            (memory, m)
          case Store(location, value) =>
            val binaryWithFloating = applyMask(mask.bits, location)
            val updatedMem = binaryWithFloating.zipWithIndex
              .collect { case ('X', idx) =>
                idx
              }
              .foldLeft(List(binaryWithFloating)) { case (addressesToWrite, idx) =>
                addressesToWrite.flatMap { addr =>
                  val address1 = addr.clone()
                  address1(idx) = '1'
                  val address2 = addr.clone()
                  address2(idx) = '0'
                  List(address1, address2)
                }
              }
              .foldLeft(memory) { case (mem, address) =>
                mem + (bitArrayToLong(address) -> value)
              }
            (updatedMem, mask)
        }
      }
      ._1
      .collect {
        case (_, value) if value != 0 => value
      }
      .sum
  }

  sealed trait Command

  case class Store(location: Int, value: Long) extends Command

  case class Mask(bits: Map[Int, Char])        extends Command

}
