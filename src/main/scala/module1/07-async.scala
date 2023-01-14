package module1

import module1.threads.async
import module1.utils.NameableThreads

import java.io.File
import java.util.{Timer, TimerTask}
import java.util.concurrent.{Callable, Executor, ExecutorService, Executors, ForkJoinPool, ThreadFactory, ThreadPoolExecutor}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise, TimeoutException}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try, Using}

object threads {


  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from " +
        s"${Thread.currentThread().getName}")
    }
  }



  def printRunningTime(v: =>Unit): Unit = {
    val start = System.currentTimeMillis()
    v
    val end = System.currentTimeMillis()
    println(s"Execution time ${end - start}")
  }

  def getRatesLocation1 = async{
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = async{
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t =  new Thread{
      override def run(): Unit = {
        v = f
      }
    }
    t.start()
    t.join()
    v
  }

  def getRatesLocation3 = async2{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }

  def getRatesLocation4 = async2{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }

  class ToyFuture[T] private(v: () => T){

    private var isCompleted: Boolean = false
    private val q = mutable.Queue[T => _]()
    private var r: T = null.asInstanceOf[T]

    def map[B](f: T => B): ToyFuture[B] = ???
    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???

    def onComplete[U](f: T => U): Unit = {
      if(isCompleted) f(v())
      else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          r = v()
          isCompleted = true
          while(q.nonEmpty){
            q.dequeue()(r)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](v: => T)(executor: Executor): ToyFuture[T] = {
      val f = new ToyFuture[T](() => v)
      f.start(executor)
      f
    }
  }

  def getRatesLocation5: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(executor.pool1)

  def getRatesLocation6: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(executor.pool2)





}

object executor {
      val pool1: ExecutorService = Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService = Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService = Executors.newWorkStealingPool(4)
      val pool4: ExecutorService = Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}

object tryObj{


  def readFromFile(): List[Int] = {
    val s: BufferedSource =
      Source.fromFile(new File("ints.txt"))
    val result: List[Int] = try{
      s.getLines().toList.map(_.toInt)
    } catch {
      case e =>
        println(e.getMessage)
        Nil
    } finally {
      s.close()
    }

    result
  }




  def readFromFile2(): Try[List[Int]] = {
    val source: Try[BufferedSource] =
      Try(Source.fromFile(new File("ints.txt")))

    def lines(s: Source) = Try(s.getLines().toList.map(_.toInt))

    val r: Try[List[Int]] = for{
      s <- source
      v <- lines(s)
    } yield v

    val r2: Try[List[Int]] = source.flatMap(s => lines(s).map(v => v))

    source.foreach(_.close)
    r
  }

}

object future{

  val f1 = Future.successful(10)
  val f2 = Future.failed(new Exception())
  val f3 = Future.fromTry(Try())



  val getRatesLocation1 = Future{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(scala.concurrent.ExecutionContext.global)

  val getRatesLocation2 = Future{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(scala.concurrent.ExecutionContext.global)

  def printRunningTime[T](v: => Future[T]): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    for{
      start <- Future.successful(System.currentTimeMillis())
      r <- v
      end <- Future.successful(System.currentTimeMillis())
      _ <-  Future.successful(println(s"Execution time ${end - start}"))
    } yield r
  }

  val r4 = getRatesLocation1.map(i => i + 10)(scala.concurrent.ExecutionContext.global)
  val r5 = getRatesLocation1.foreach(i => i + 10)(scala.concurrent.ExecutionContext.global)

  val r6= getRatesLocation1.onComplete{
    case Success(value) => println("")
    case Failure(exception) => println("")
  }(scala.concurrent.ExecutionContext.global)

  val r7 = getRatesLocation1.recover{
    case e => 0
  }(scala.concurrent.ExecutionContext.global)


  val ec = ExecutionContext.fromExecutor(executor.pool1)
  val ec2 = ExecutionContext.fromExecutor(executor.pool2)
  val ec3 = ExecutionContext.fromExecutor(executor.pool3)
  val ec4 = ExecutionContext.fromExecutor(executor.pool4)

  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action ${v} in ${Thread.currentThread().getName}")
    v
  }

  val f6 = Future(action(10))(ec)
  val f7 = Future(action(20))(ec2)

  import scala.concurrent.ExecutionContext.Implicits.global

  val f8: Future[Int] = f6.flatMap{ v1 =>
    action(50)
    f7.map{v2 =>
      action(v1 + v2)
    }
  }

  for{
    v1 <- f6
    _ = action(50)
    v2 <- f7
  } yield action(v1 + v2)

}

object promise{

  val p1: Promise[Int] = Promise[Int]
  val f1: Future[Int] = p1.future


  object FutureSyntax {

    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    def map[T, B](future: Future[T])(f: T => B): Future[B] = {
      val p = Promise[B]
      future.onComplete {
        case Failure(exception) => p.failure(exception)
        case Success(value) => p.success(f(value))
      }
      p.future
    }

    def flatMap[T, B](future: Future[T])(f: T => Future[B]): Future[B] = ???


    def make[T](v: => T)(implicit ec: ExecutionContext): Future[T] = {
      val p: Promise[T] = Promise[T]
      val r = new Runnable {
        override def run(): Unit = p.complete(Try(v))
      }
      ec.execute(r)
      p.future
    }

    def make[T](v: => T, timeout: Long): Future[T] = {
      val p = Promise[T]
      val timer = new Timer(true)
      val task = new TimerTask {
        override def run(): Unit = ???
      }
      timer.schedule(task, timeout)
      ???
    }
  }
}
