package collections

import scala.annotation.tailrec
import scala.util.Random

class BallsExperiment {
  private val balls: List[Char] = List(1, 0, 1, 0, 1, 0);

  def isFirstBlackSecondWhite: Boolean = {
    val randomIdx = Random.nextInt(6)
    val firstBall = balls(randomIdx)
    val secondBall = balls
      .zipWithIndex
      .filter(_._2 != randomIdx )
      .map(_._1)(Random.nextInt(5))

    if (firstBall == 0 && secondBall == 1) true
    else false
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = getListOfExperiments(count, List())

    val countOfExperiments = listOfExperiments.map(be => be.isFirstBlackSecondWhite)
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }

  @tailrec
  private def getListOfExperiments(count: Int, list: List[BallsExperiment]): List[BallsExperiment] = {
    if (count == 0) return  list
    val ballsExperiment: BallsExperiment = new BallsExperiment
    getListOfExperiments(count - 1,  ballsExperiment :: list)
  }
}