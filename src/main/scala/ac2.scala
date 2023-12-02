import scala.io.Source


object ac2 extends App {

  val filename = "input/in2"
  val xs = for (
    line <- Source.fromFile(filename).getLines
  ) yield line

  val colorPattern = "(\\d+)\\s(\\w+)".r
val integerPattern = "\\d+".r

val games = xs.map( line =>{
  // Extract color counts from the line
  val colorCounts = colorPattern.findAllMatchIn(line).toList
  // Group colors and sum their counts
  val groupedColors =
    colorCounts.groupBy(_.group(2)).view.mapValues(_.map(_.group(1).toInt).max)
  // Return game name and color counts
  line.takeWhile(_ != ':').trim -> groupedColors
}
)
println(games.map(_._2.map(_._2).toList.product).sum)
 
}

