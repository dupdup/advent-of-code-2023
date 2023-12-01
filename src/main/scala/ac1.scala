import scala.io.Source


object ac1 extends App {

  val filename = "input/in1_1"
  val xs = for (
    line <- Source.fromFile(filename).getLines
  ) yield line
val digitMap = Map(
  "one" -> "o1e",
  "two" -> "t2o",
  "three" -> "t3e",
  "four" -> "f4r",
  "five" -> "5",
  "six" -> "s6x",
  "seven" -> "s7n",
  "eight" -> "e8t",
  "nine" -> "n9e"
)
// Function to calculate calibration value for a given line
def calculateCalibrationValue(line: String): Int = {
  print(line)
  val digits = "\\d".r.findAllIn(replaceSpelledOutDigits(line)).toList
    // Combine the first and last digits to form a two-digit number
  println("--",digits.head,digits.last)
  s"${digits.head}${digits.last}".toInt
}
def replaceSpelledOutDigits(line: String): String = {
  var replacedLine = line
  digitMap.foreach { case (word, digit) =>
    replacedLine = replacedLine.replace(word, digit)
  }
  replacedLine
}

// Calculate calibration values for all lines and sum them
val sumOfCalibrationValues = xs.map(calculateCalibrationValue).sum

println(s"Sum of Calibration Values: $sumOfCalibrationValues")
  


}

