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