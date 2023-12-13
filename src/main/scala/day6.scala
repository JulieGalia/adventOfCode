import scala.math._
import scala.collection.immutable.NumericRange


object day6 {

  val example = """Time:      7  15   30
                  |Distance:  9  40  200""".stripMargin

  // delta= b^2 - 4ac
  // b = time, c= distance a= 1

  val input6 = """Time:        56     97     78     75
                |Distance:   546   1927   1131   1139""".stripMargin

  case class Race(time : Long, record: Long)

  case class RaceSolution( s1: Long, s2: Long, range: NumericRange.Inclusive[Long])

  def pow(nb: Long, puiss: Double): Double = {
    scala.math.pow(nb.toDouble, puiss)
  }

  def sqrt(x: Double): Double = {
    scala.math.sqrt(x)
  }

  def parser(input: String): Array[Race] = {
    val lines = input.split("\n")
    val time: Array[Long] = lines.filter(_.startsWith("Time")).head.split("Time:").tail.map(_.trim).head
      .split(" ").filter(_.nonEmpty).map(_.toLong)

    val distance: Array[Long] = lines.filter(_.startsWith("Distance")).head.split("Distance:").tail.map(_.trim)
      .head.split(" ").filter(_.nonEmpty).map(_.toLong)

    time.zip(distance).map(x=> Race(x._1 , - x._2)) // negatif car polynome : ax^2 + bx - c  , record = c

  }

  def parser2(input: String): Array[Race] = {
    val lines = input.split("\n")
    val time  = lines.filter(_.startsWith("Time")).head.split("Time:").tail.head
      .replace(" ", "").toLong


    val distance = lines.filter(_.startsWith("Distance")).head.split("Distance:").tail
      .head.replace(" ", "").toLong


    Array(Race(time, -(distance))) // negatif car polynome : ax^2 + bx - c  , record = c

  }


  def calculateSolution(race : Race): RaceSolution = {
    val delta: Double = pow(race.time.toLong,2) - ( 4*(-1 * race.record))
    val s1 = ((-race.time + sqrt(delta)) / - 2).floor.toLong + 1 // fix edge cases where s integer
    val s2 = ((-race.time - sqrt(delta)) / - 2).ceil.toLong - 1

    RaceSolution(s1, s2, s1 to s2)

  }

  def p1(input: String ):Int = {
    val races: Array[Race] = parser(input)
    races.map(calculateSolution).map(_.range.length).product
  }

  print(p1(example))
  print(p1(input6))

  def p2(input: String): Int = {
    val races: Array[Race] = parser2(input)
    races.map(calculateSolution).map(_.range.length).head
  }

  print(p2(example))
  print(p2(input6))

}
