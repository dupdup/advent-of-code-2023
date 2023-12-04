import scala.io.Source

object ac3 extends App {

  val filename = "input/in3"
  val xs = Source.fromFile(filename).getLines.toList
  val input = xs.mkString.replaceAll("\n", "") // """sssss"""
  val symbolPattern = """[^0-9.]""".r
  val symbols = symbolPattern.findAllIn(input).toList.distinct

  val part1 = xs.zipWithIndex
    .flatMap(tup =>
      extractNumbers(tup._1).filter(x =>
        hasSymbolOnNeighbor(tup._2, x._2, x._1.toString())
      )
    )

  var starIndices = for {
    (line, lineIndex) <- xs.zipWithIndex
    charIndex <- line.indices if line(charIndex) == '*'
  } yield (lineIndex , charIndex )

  val part2 = starIndices
    .map(x => {val t = neighborNumbers(x._1, x._2)
    println(t)
    t
    })
    .filter(_.size == 2)
    .map(ts => ts(0) * ts(1))
    .sum
    
  def neighborNumbers(lineIndex:Int,charIndex:Int):List[Int] = {
    val res = lineIndex match {
      case 0 => extractMatchingNumbers(xs(lineIndex),charIndex):::extractMatchingNumbers(xs(lineIndex+1),charIndex)
      case 139 => extractMatchingNumbers(xs(lineIndex),charIndex):::extractMatchingNumbers(xs(lineIndex-1),charIndex)
      case _ => extractMatchingNumbers(xs(lineIndex),charIndex):::extractMatchingNumbers(xs(lineIndex-1),charIndex):::extractMatchingNumbers(xs(lineIndex+1),charIndex)
    }
    res
  }
  def extractMatchingNumbers(line:String, charIndex:Int) = {
    extractNumbers(line).filter(x => {
     (x._2 to  x._2+(x._1.toString().length()-1)).intersect(charIndex-1 to charIndex+1).nonEmpty
    }).map(_._1)
  }

  println("sss",part2);

  def extractNumbers(input: String): List[(Int, Int)] = {
    val pattern = "\\d+".r

    val matches = pattern.findAllMatchIn(input)
    // println(matches.toList)
    matches.map { matchResult =>
      val number = matchResult.group(0).toInt
      val startIndex = matchResult.start
      (number, startIndex)
    }.toList

  }

  def hasSymbolOnNeighbor(
      lineIndex: Int,
      charIndex: Int,
      number: String
  ): Boolean = {
    hasSymbol(lineIndex, charIndex, number.length()) ||
    (lineIndex != 0 && hasSymbol(
      lineIndex - 1,
      charIndex,
      number.length()
    )) || (lineIndex != xs.size - 1 && hasSymbol(
      lineIndex + 1,
      charIndex,
      number.length()
    ))
  }
  def hasSymbol(lineIndex: Int, charIndex: Int, lenght: Int): Boolean = {
    // println(s"$lineIndex, $charIndex, $lenght")
    var preCharIndex = charIndex
    if (charIndex > 0)
      preCharIndex = charIndex - 1;
    symbols
      .intersect(
        (xs.toList(lineIndex) + "..")
          .substring(preCharIndex, charIndex + lenght + 1)
          .split("")
      )
      .size > 0
  }

}
