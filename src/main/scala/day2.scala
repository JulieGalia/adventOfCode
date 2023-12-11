object day2 {
  def parseThresold(set: Array[String]): String = {
    val mapSet: Array[(Int, String)] = set.map(x => (x.split(" ").head.toInt, x.split(" ").last))

    val resSet: Array[String] = mapSet.map(x => x._2 match {
      case "green" => if (x._1 < 14) "ok" else "KO"
      case "red" => if (x._1 < 13) "ok" else "KO"
      case "blue" => if (x._1 < 15) "ok" else "KO"
      case _ => "error"
    }
    )

    val resGame = if (resSet.contains("ko")) "impossible" else "possible"
    resGame

  }


}
