import module1.threads.{Thread1, ToyFuture, getRatesLocation1, getRatesLocation2, getRatesLocation3, getRatesLocation4, getRatesLocation5, getRatesLocation6, printRunningTime}
import module1.{functions, type_system}

object Main{


  def main(args: Array[String]): Unit = {

//    val t1 = new Thread1
//    val t2 = new Thread1
//    t1.start()
//    t1.join()
//    t2.start()

    def rates = {

      getRatesLocation5.onComplete{ i1 =>
        getRatesLocation6.onComplete{ i2 =>
          println(i1 + i2)
        }
      }

      for{
        i1 <- getRatesLocation5
        i2 <- getRatesLocation6

      } yield i1 + i2

    }

    printRunningTime(rates)

    println(s"Hello, from ${Thread.currentThread().getName}")
  }

}