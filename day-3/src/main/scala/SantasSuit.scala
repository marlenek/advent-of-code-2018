package main.scala

import scala.io.Source

object SantasSuit {

  case class claim(id: Int, left: Int, top: Int, wide: Int, tall: Int) {}

  def readClaims(fileName: String): Seq[claim] = {
    for {
      line <- Source.fromResource(fileName).getLines().toVector
      values = line.split("[,:x@#]").map(_.trim)
    } yield claim(values(1).toInt, values(2).toInt, values(3).toInt, values(4).toInt, values(5).toInt)
  }

  def calculateFabricSize(claims: Seq[claim]): (Int, Int) = {
    var maxWide, maxTall = 0;
    claims.foreach { f =>
      if (f.left + f.wide > maxWide) maxWide = f.left + f.wide
      if (f.top + f.tall > maxTall) maxTall = f.top + f.tall
    }
    (maxWide, maxTall)
  }

  def createClaimsMatrix(claims: Seq[claim], wide: Int, tall: Int): Array[Array[Int]] = {
    var fabric = Array.fill(wide + 1, tall + 1)(0)
    claims.foreach { f =>
      for (i <- f.left to f.wide + f.left - 1) {
        for (j <- f.top to f.tall + f.top - 1) {
          fabric(i)(j) += 1
        }
      }
    }
    fabric
  }

  def calculateFabricOverlaps(claims: Seq[claim], wide: Int, tall: Int, fabric: Array[Array[Int]]): Int = {
    var result = 0
    for (i <- 0 to wide) {
      for (j <- 0 to tall) {
        if (fabric(i)(j) >= 2)
          result += 1
      }
    }
    result
  }

  def findNotOverlappingClaim(claims: Seq[claim], wide: Int, tall: Int, fabric: Array[Array[Int]]): Option[Int] = {
    claims.foreach { f =>
      var flag = true
      for (i <- f.left to f.wide + f.left - 1) {
        for (j <- f.top to f.tall + f.top - 1) {
          if (fabric(i)(j) != 1)
            flag = false
        }
      }
      if (flag) return Some(f.id)
    }
    None
  }

  def main(args: Array[String]): Unit = {
    val claims = readClaims("input.txt")
    val fabricSize = calculateFabricSize(claims)
    var fabricMatrix = createClaimsMatrix(claims, fabricSize._1, fabricSize._2)
    println("Result for part 1: " + calculateFabricOverlaps(claims, fabricSize._1, fabricSize._2, fabricMatrix))
    println("Result for part 2: " + findNotOverlappingClaim(claims, fabricSize._1, fabricSize._2, fabricMatrix))
  }
}