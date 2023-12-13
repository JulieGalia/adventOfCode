import scala.math._
object day6 {

  val example = """Time:      7  15   30
                  |Distance:  9  40  200""".stripMargin

  // delta= b^2 - 4ac
  // b = time, c= distance a= 1

  case class Race(time : Int, record: Int)

  def parser(input: String) = {
    val lines = example.split("\n")
    val time: Array[Int] = lines.filter(_.startsWith("Time")).head.split("Time:").tail.map(_.trim).head
      .split(" ").filter(_.nonEmpty).map(_.toInt)

    val distance: Array[Int] = lines.filter(_.startsWith("Distance")).head.split("Distance:").tail.map(_.trim)
      .head.split(" ").filter(_.nonEmpty).map(_.toInt)

    val races = time.zip(distance).map(x=> Race(x._1 , x._2))

  }

}
