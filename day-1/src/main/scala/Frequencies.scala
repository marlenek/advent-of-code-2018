import scala.io.Source

object Frequencies {

  def frequenciesPart1(changesOfFrequency: List[Int]): Int = changesOfFrequency.reduce(_ + _)

  def frequenciesPart2(changesOfFrequency: List[Int]): Int = {
    var frequencies = changesOfFrequency.scan(0)(_ + _)
    while (frequencies.diff(frequencies.distinct).isEmpty) {
      frequencies = addOneLoopToFrequencies(changesOfFrequency, frequencies)
    }
    frequencies.diff(frequencies.distinct).head
  }

  def addOneLoopToFrequencies(frequenciesList: List[Int], frequencies: List[Int]): List[Int] = {
    frequencies ::: frequenciesList.scan(frequencies.last)(_ + _).tail
  }

  def main(args: Array[String]): Unit = {

    val changesOfFrequency = Source.fromResource("input.txt").getLines().map(_.toInt).toList
    println("Result for part 1: " + frequenciesPart1(changesOfFrequency))
    println("Result for part 2: " + frequenciesPart2(changesOfFrequency))
  }
}