import scala.io.Source
import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters._

object ac6 extends App {

    val raceTimes = List(41667266)
    val recordDistances = List(BigInt("244104712281040"))
  
    val waysToBeatRecord = raceTimes.zip(recordDistances).map { case (time, distance) =>
      calculateWaysToBeatRecord(time, distance)
    }

    val productOfWays = waysToBeatRecord.product
    println(s"$waysToBeatRecord The product of ways to beat the record in each race is: $productOfWays")

  def calculateWaysToBeatRecord(time: BigInt, distance: BigInt): BigInt = {
    val maxHoldTime = time - 1
    val ways = (BigInt(1) to maxHoldTime).par.count(holdTime => (holdTime * (time - holdTime)) > distance)
    ways
  }
}
