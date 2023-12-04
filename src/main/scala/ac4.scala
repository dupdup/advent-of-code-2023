import scala.io.Source

object ac4 extends App {

  val filename = "input/in4"
  val lines = Source.fromFile(filename).getLines.toList
// Split the card into two sets of numbers
  val mcs = lines
    .map(line => {
      val sets = line
        .split(":")(1)
        .split('|')
        .map(_.trim.split("\\s+").map(_.toInt).toSet)
      sets(0).intersect(sets(1)).size
    })
  var cards = Array.fill(lines.size)(1)
  for ((e, i) <- mcs.zipWithIndex) {
    (i+1 to i+e)
      .foreach(x=>cards(x)=cards(i)+cards(x))  
    }
  println(cards.toList,cards.sum)

}
