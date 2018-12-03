package main.scala

import scala.io.Source

object SantasSuit {

  def fabricOverlap(claims: Seq[fabric], wide: Int, tall: Int): Int = {
    var fabric = Array.fill(wide+1, tall+1)(0)
    claims.foreach { f =>
      for (i <- f.left to f.wide + f.left) {
        for (j <- f.top to f.tall + f.top) {
          fabric(i)(j) += 1
        }
      }
    }
    var result = 0
    for (i <- 0 to wide ) {
      for (j <- 0 to tall ) {
        if (fabric(i)(j) >= 2)
          result += 1
      }
    }
    result
  }

  def part2(boxesList: List[String]): String = {
    ""
  }

  case class fabric(left: Int, top: Int, wide: Int, tall: Int) {}

  def getFabricSize(claims: Seq[fabric]): (Int, Int) = {
    var maxWide, maxTall = 0;
    claims.foreach { f =>
      if (f.left + f.wide > maxWide) maxWide = f.left + f.wide
      if (f.top + f.tall > maxTall) maxTall = f.top + f.tall
    }
    (maxWide, maxTall)
  }

  def readSales(fileName: String): Seq[fabric] = {
    for {
      line <- Source.fromResource(fileName).getLines().toVector
      values = line.split("[,:x@]").map(_.trim)
    } yield fabric(values(1).toInt, values(2).toInt, values(3).toInt, values(4).toInt)
  }

  def main(args: Array[String]): Unit = {
    val boxesList = readSales("input.txt")
    println("Result for part 1: " + fabricOverlap(boxesList, getFabricSize(boxesList)._1, getFabricSize(boxesList)._2))
    //   println("Result for part 2: " + part2(boxesList))
  }
}