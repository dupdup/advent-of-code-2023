import scala.io.Source
import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters._

object ac5 extends App {

  val filename = "input/in5"
  val lines = Source.fromFile(filename).mkString

  val seeds = "2149186375 163827995 1217693442 67424215 365381741 74637275 1627905362 77016740 22956580 60539394 586585112 391263016 2740196667 355728559 2326609724 132259842 2479354214 184627854 3683286274 337630529"
      .split(" ")
      .map(BigInt(_))
      .toList

  var part2 = seeds.grouped(2).map {
    case List(a, b) => (a to (a + b - 1))
  }

// Split by empty lines
  val outerList = lines.split("\n\n").map { innerString =>
    // Split inner string by new lines and map to ranges
    innerString.split("\n").map { line =>
      val Array(first, second, last) = line.split(" ").map(BigInt(_))
      (second, (second + last - 1), first - second)
    }
  }

  def mapIntegers(
      range: NumericRange.Inclusive[BigInt],
      rangesAndDiffs: Array[Array[
        (BigInt, BigInt, BigInt)
      ]]
  ): BigInt = {

    var res = BigInt(0)
    range.foreach(x => {
      var min = rangesAndDiffs.foldLeft(x) { (accInt, innerList) =>
        innerList.find { case (a, b, c) => a <= accInt && accInt <= b } match {
          case Some((_, _, diff)) => accInt + diff
          case None               => accInt
        }
      }
      if (res == 0 || res > min) {
        res = min
      }
    })
    res
  }

  var xs = part2.toList.par.map(range => mapIntegers(range, outerList))
  println(xs.min, xs.toList)

}
