import scala.collection.immutable.NumericRange
import scala.util.{Failure, Success, Try}

object day5 {
  val input: String = """seeds: 4239267129 20461805 2775736218 52390530 3109225152 741325372 1633502651 46906638 967445712 47092469 2354891449 237152885 2169258488 111184803 2614747853 123738802 620098496 291114156 2072253071 28111202
                |
                |seed-to-soil map:
                |803774611 641364296 1132421037
                |248421506 1797371961 494535345
                |1936195648 2752993203 133687519
                |2069883167 2294485405 458507798
                |2804145277 283074539 358289757
                |3162435034 2886680722 1132532262
                |2528390965 4019212984 275754312
                |766543479 248421506 34653033
                |742956851 1773785333 23586628
                |801196512 2291907306 2578099
                |
                |soil-to-fertilizer map:
                |2497067833 718912393 1047592994
                |3544660827 4222700866 72266430
                |770426288 3365742958 209338740
                |3698421476 2775964622 508284117
                |1441878450 1818019282 725791090
                |417593992 265113557 15217985
                |979765028 3760587444 462113422
                |2167669540 2543810372 143892547
                |3616927257 3284248739 81494219
                |4206705593 2687702919 88261703
                |2380194851 3575081698 116872982
                |0 280331542 15942291
                |718912393 1766505387 51513895
                |152480435 0 265113557
                |2311562087 3691954680 68632764
                |15942291 296273833 136538144
                |
                |fertilizer-to-water map:
                |0 402310798 253353164
                |778924681 2773042028 194127973
                |2853824225 2967170001 585461563
                |3827117536 3909653920 385313376
                |4259877071 3552631564 35090225
                |973052654 3635167948 222704323
                |253353164 0 389964349
                |2230088185 778924681 571954391
                |1195756977 1490392659 342200935
                |2802042576 3857872271 51781649
                |643317513 389964349 12346449
                |4212430912 3587721789 47446159
                |3439285788 2385210280 387831748
                |1677471499 1832593594 552616686
                |1537957912 1350879072 139513587
                |
                |water-to-light map:
                |1548505089 767179152 4433418
                |3833169479 2956286720 133538400
                |2966709060 3309731935 102304094
                |1552938507 844050660 203612289
                |4257043426 3089825120 37923870
                |2862957901 3567999512 28008008
                |127112704 319767838 4466599
                |840317941 174506417 34039792
                |2890965909 3596007520 40520529
                |15787022 2007458428 111325682
                |2398090681 21771313 152735104
                |1094590916 1294380254 4387553
                |517844904 840169267 3881393
                |2556445662 1535118242 8735340
                |1266005567 2376897884 172496096
                |874357733 1314885059 220233183
                |3696946976 2820064217 136222503
                |2271345339 208546209 111221629
                |703336145 477538609 136981796
                |389299157 1710880680 59057725
                |4183266377 2766992510 22982117
                |521726297 324234437 53105792
                |1438501663 1881931289 110003426
                |131579303 1298767807 16117252
                |2102535156 614520405 152658747
                |0 2549393980 15787022
                |1098978469 1543853582 167027098
                |3966707879 2789974627 30089590
                |2255193903 0 16151436
                |1756550796 377340229 100198380
                |574832089 2360386712 16511172
                |2382566968 1991934715 15523713
                |3069013154 3636528049 627933822
                |2766992510 3178543922 79332992
                |2931486438 3274509313 35222622
                |3996797469 4264461871 30505425
                |2846325502 3257876914 16632399
                |2033978459 771612570 68556697
                |4206248494 3127748990 50794932
                |2550825785 16151436 5619877
                |591343261 1769938405 111992884
                |448356882 1047662949 69488022
                |4027302894 3412036029 155963483
                |147696555 2118784110 241602602
                |1856749176 1117150971 177229283
                |
                |light-to-temperature map:
                |2549521624 1806050718 400234502
                |1279003707 1469066403 336984315
                |2063720323 2518736018 367281175
                |4240496851 236622733 54470445
                |3737038415 1201359870 20798035
                |1170741345 1222157905 108262362
                |1925074187 1330420267 138646136
                |3757836450 291093178 323945285
                |3424587617 2206285220 312450798
                |236622733 2886017193 934118612
                |4138496410 1042644754 102000441
                |4081781735 1144645195 56714675
                |2431001498 615038463 118520126
                |1615988022 733558589 309086165
                |2949756126 3820135805 474831491
                |
                |temperature-to-humidity map:
                |725888341 86282489 843183510
                |3782717746 1630698708 99613080
                |2529768467 2786969418 347392693
                |2195908552 2059541517 89214959
                |3062107482 2168182310 90554707
                |1730470902 3134362111 465437650
                |2964061476 2688923412 98046006
                |2285123511 2358509211 13167510
                |2877161160 3875960109 61807956
                |0 929465999 639605852
                |3484769060 2148756476 19425834
                |2298291021 1730311788 170053852
                |639605852 0 86282489
                |3504194894 2371676721 119346975
                |4275382932 3599799761 19584364
                |2468344873 2491023696 61423594
                |3623541869 1900365640 159175877
                |4138906810 2552447290 136476122
                |3918976473 3656029772 219930337
                |2938969116 4269874936 25092360
                |3882330826 3619384125 36645647
                |3152662189 3937768065 332106871
                |1630698708 2258737017 99772194
                |
                |humidity-to-location map:
                |1426868383 2786540732 64165562
                |1639911414 2027746720 730664673
                |857589555 0 114197007
                |2370576087 1887556908 140189812
                |3396523523 1265337150 488817864
                |1491033945 2850706294 148877469
                |3885341387 2999583763 409625909
                |0 114197007 857589555
                |1293466489 1754155014 133401894
                |2510765899 3409209672 885757624
                |1265337150 2758411393 28129339""".stripMargin
  val exemple: String =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4
      |""".stripMargin

  case class Source(x: Long)
  case class RangeSource(interval: NumericRange.Exclusive[Long], length: Long )
  case class Almanac (intervalSource: NumericRange.Exclusive[Long], intervalDest: NumericRange.Exclusive[Long], length:Long)
  case class SlicedAlmanac (newIntervalSource : NumericRange.Exclusive[Long], oldAlmanac : Almanac )
  case class AlmanacMap (linesAlmanac: Array[Almanac], name: String)


  def getSeeds(almanacArray: Array[String]): List[Source] = {
    val seedLine: Array[String] = almanacArray.filter(_.startsWith("seeds:"))
    seedLine(0).split("seeds:").tail.head.trim.split(" ").map(x => Source(x.toLong)).toList
  }

  def getRangeSeeds(almanacArray: Array[String]) : List[RangeSource] = {
    val seedLine:  Array[String] = almanacArray.filter(_.startsWith("seeds:"))
    val rawSeeds = seedLine(0).split("seeds:").tail.head.trim.split(" ").map(x => x.toLong).sliding(2,2).toList
    val rangeSeeds: List[RangeSource] = rawSeeds.map(x => RangeSource(x(0) until x(0) + x(1), x(1)))
    rangeSeeds
  }


  def parseAlmanac(input : String): AlmanacMap = {
    val lines = input.split("\n")
    val name = lines(0).replace(":", "").trim
    val linesAlmanac: Array[Almanac] = lines.tail.map(_.split(" ").map(x => x.toLong)).map(x=> Almanac(x(1) until x(1) + x(2), x(0) until  x(0) + x(2), x(2)))
    AlmanacMap(linesAlmanac, name)
  }


  def findDestination(value: Source, listInterval : AlmanacMap): Source = {
    val validAlmanac: Option[Almanac] = listInterval.linesAlmanac.find(x => value.x >= x.intervalSource.start && value.x <= x.intervalSource.end)
    if(validAlmanac.isEmpty){
      value
      }else{
      val delta: Long = value.x - validAlmanac.get.intervalSource.start
      val destination: Long = delta + validAlmanac.get.intervalDest.start
      Source(destination)
    }

  }


  def calculateIntervalDestinationFromRangeSource(slicedAlmanac: SlicedAlmanac): RangeSource = {
    val delta: Long = (slicedAlmanac.newIntervalSource.start) - (slicedAlmanac.oldAlmanac.intervalSource.start)
    val destinationStart: Long = delta + slicedAlmanac.oldAlmanac.intervalDest.start
    val destinationRange = destinationStart until destinationStart + slicedAlmanac.newIntervalSource.length
    RangeSource(destinationRange, slicedAlmanac.newIntervalSource.length)
  }



  def sliceRangeSource(almanac: Almanac, rangeSource: RangeSource) :NumericRange.Exclusive[Long] = {
    val mapIntervalSource: NumericRange.Exclusive[Long] = almanac.intervalSource match {
      case sourceInside if sourceInside.contains(rangeSource.interval.start) && sourceInside.contains(rangeSource.interval.end)  => rangeSource.interval
      case startInside if startInside.contains(rangeSource.interval.start) => (rangeSource.interval.start) until startInside.end
      case endInside if endInside.contains(rangeSource.interval.end) => (endInside.start) until  rangeSource.interval.end
      case almanacInside if (rangeSource.interval.start) < (almanacInside.start) && (almanacInside.end) < rangeSource.interval.end => almanac.intervalSource
  }
    mapIntervalSource
  }

  def findNextInterval (previousEnd: Long, nextAlmanac: Try[Almanac], originRangeSource: RangeSource): Any = {
    nextAlmanac match {
      case Success(nextValidAlmanac) =>
        val nextStart = nextValidAlmanac.intervalSource.start
        val delta = nextStart - previousEnd
        delta match {
          case ok if (delta <= 0) => val newSourceInterval = sliceRangeSource(nextValidAlmanac,originRangeSource)
            SlicedAlmanac(newSourceInterval, nextValidAlmanac)
          case _ => val noMapInterval: NumericRange.Exclusive[Long] = previousEnd until nextValidAlmanac.intervalSource.start
            RangeSource(noMapInterval, noMapInterval.length)
        }

      case Failure(exception) => val noEndMapInterval: NumericRange.Exclusive[Long] = previousEnd until originRangeSource.interval.end
        RangeSource(noEndMapInterval, noEndMapInterval.length)
    }

  }

  def getRangeDestination(rangeSource: RangeSource, listInterval: AlmanacMap): List[RangeSource] = {
    //print(s"range source $rangeSource \n")
    val validAlmanac: Array[Almanac] = listInterval.linesAlmanac.filter(x => x.intervalSource.contains(rangeSource.interval.start)
      || x.intervalSource.contains(rangeSource.interval.end) ||
      ((rangeSource.interval.start) < (x.intervalSource.start) && (x.intervalSource.end) < rangeSource.interval.end))
      .sortBy(_.intervalSource.start)
    if (validAlmanac.isEmpty) {
      List(rangeSource)
    } else {
      var newRangeSource: List[RangeSource]= List.empty[RangeSource]
      var previousEnd = rangeSource.interval.start
      var indexValidAlmanac = 0
      //var i = 1
      do{
        //print(s"I : $i \n")
        val newInterval: Any = findNextInterval(previousEnd, Try(validAlmanac(indexValidAlmanac)), rangeSource)
        //print(s"index valid $indexValidAlmanac \n")
        //print(s"new Intervalle : $newInterval and $previousEnd \n ")

        val newSource: RangeSource = newInterval match {
          case noMapDestination: RangeSource =>
            previousEnd = noMapDestination.interval.end
            noMapDestination
          case slicedAlmanac: SlicedAlmanac =>
            indexValidAlmanac += 1
            previousEnd = slicedAlmanac.newIntervalSource.end
            calculateIntervalDestinationFromRangeSource(slicedAlmanac)
        }

       // print(s"new previous $previousEnd")
        newRangeSource = newRangeSource ++ List(newSource)

      } while (previousEnd < rangeSource.interval.end )

      newRangeSource

    }

  }


  def getDestinationFromSource(sources : List[Source], almanacMap: AlmanacMap):List[Source] = {
    sources.map(x => findDestination(x, almanacMap))
  }


  def p1(input: String): Long = {
    val almanacArray: Array[String] = input.split("\n\n")

    val seeds: List[Source] = getSeeds(almanacArray)

    val almanacList: Array[AlmanacMap] = almanacArray.filterNot(_.startsWith("seeds:")).map(parseAlmanac)

    val soil: List[Source] = getDestinationFromSource(sources = seeds, almanacList.filter(_.name == "seed-to-soil map").head)

    val fertilizer = getDestinationFromSource(sources = soil, almanacList.filter(_.name == "soil-to-fertilizer map").head)

    val water = getDestinationFromSource(sources = fertilizer, almanacList.filter(_.name == "fertilizer-to-water map").head)

    val light = getDestinationFromSource(sources = water, almanacList.filter(_.name == "water-to-light map").head)

    val temperature = getDestinationFromSource(sources = light, almanacList.filter(_.name == "light-to-temperature map").head)

    val humidity = getDestinationFromSource(sources = temperature, almanacList.filter(_.name == "temperature-to-humidity map").head)

    val location = getDestinationFromSource(sources = humidity, almanacList.filter(_.name == "humidity-to-location map").head)

    location.map(_.x).min
  }

  def p2 (input: String): Long = {
    val almanacArray: Array[String] = input.split("\n\n")

    val seedsRange: List[RangeSource] = getRangeSeeds(almanacArray)

    val almanacList: Array[AlmanacMap] = almanacArray.filterNot(_.startsWith("seeds:")).map(parseAlmanac)

    val soilRangeList: List[RangeSource] = seedsRange.flatMap(seeds => getRangeDestination(seeds, almanacList.filter(_.name == "seed-to-soil map").head))

    val fertilizerRangeList: List[RangeSource] = soilRangeList.flatMap(soil => getRangeDestination(soil, almanacList.filter(_.name == "soil-to-fertilizer map").head)) // "soil-to-fertilizer map"

    val waterRangeList =  fertilizerRangeList.flatMap(fertilizer => getRangeDestination(fertilizer, almanacList.filter(_.name == "fertilizer-to-water map").head))

    val lightRangeList =  waterRangeList.flatMap(water => getRangeDestination(water, almanacList.filter(_.name == "water-to-light map").head))

    val temperatureRangeList =  lightRangeList.flatMap(light => getRangeDestination(light, almanacList.filter(_.name == "light-to-temperature map").head))

    val humidityRangeList =  temperatureRangeList.flatMap(temperature => getRangeDestination(temperature, almanacList.filter(_.name == "temperature-to-humidity map").head))

    val locationRangeList =  humidityRangeList.flatMap(humidity => getRangeDestination(humidity, almanacList.filter(_.name == "humidity-to-location map").head))

    locationRangeList.minBy(_.interval.start).interval.start

  }


  p2(input)


}
