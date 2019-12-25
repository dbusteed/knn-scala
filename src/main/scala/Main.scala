import scala.io.Source
import scala.io.StdIn
import scala.math.sqrt
import scala.math.pow

object Main {
  
  // euclid distance for p=2
  def calcDistance(t1: (Int, Int), t2: (Int, Int)): Double = {
    sqrt( pow((t1._1 - t2._1),2) + pow((t1._2 - t2._2),2) )
  }

  def main(args: Array[String]): Unit = {

    // avoid deprec warning
    implicit val orderer = Ordering.Double.TotalOrdering

    // tuning parameter
    val k = 3

    val fileName = "/home/davis/projects/knn-scala/knn/data/gender.csv"
    val lines = Source.fromFile(fileName).getLines().drop(1).toList

    val data: List[HealthData] = lines.map { line =>
      val split = line.split(',')
      HealthData(split(0).toString, split(1).toInt, split(2).toInt)
    }.toList

    // get new observation
    print("Enter height: ")
    val newHeight = StdIn.readInt()  

    print("Enter weight: ")
    val newWeight = StdIn.readInt()

    val newData = (newHeight,newWeight)

    // calculate the distances
    val distances: List[(String, Double)] = data.map { ob =>
      val dataPoint = (ob.height, ob.weight)
      (ob.gender, calcDistance(newData, dataPoint))
    }

    // find the "k" obs with smallest distance (the neighbors)
    val nearestLabels: List[String] = distances.sortBy(_._2).take(k).map(_._1)

    // find the most occuring neighbor
    // doesn't handle multiple modes yet
    val predictedClass = nearestLabels.groupBy(x => x).view.mapValues(_.size).maxBy(_._2)._1

    println(s"\nPredicted gender: $predictedClass")

  } 

}