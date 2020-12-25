package week2

import scala.annotation.tailrec

package object day13 {

  def crt(moduliAndRemainders: List[(Long, Long)]): Long = {
    val moduliProd = moduliAndRemainders.map(_._1).product
    moduliAndRemainders.foldLeft(0L) { case (acc, (modulo, remainder)) =>
      val p = moduliProd / modulo
      acc + remainder * mulInv(p, modulo) * p
    } % moduliProd
  }

  def mulInv(a: Long, b: Long): Long = {
    @tailrec
    def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
      if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
    }
    if (b == 1) 1
    else {
      val x1 = loop(a, b, 0, 1)
      if (x1 < 0) x1 + b else x1
    }
  }

}
