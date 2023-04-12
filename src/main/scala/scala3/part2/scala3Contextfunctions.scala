package part2.scala3Contextfunctions
/*
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

object scala3Contextfunctions {
  //Как это можно использовать в реальной программе? Допустим, у вас есть какая-то
  // глубоко вложенная функция (f4 в приведенном ниже коде), и только на этом уровне вам нужен неявный контекст
  // выполнения. Без неявных параметров вы бы добавили параметр ExecutionContext к
  // каждому отдельному вызову функции,
  // а затем должны были бы позаботиться о его передаче. С неявными
  // параметрами Scala 2 вам все равно придется объявлять
  // параметр, но вы можете сделать его неявным и избежать бремени передачи его вручную.
  //
  //В Scala 3 вы можете определить тип функции Executable[T], и тогда нам даже не
  // нужно называть неявный параметр, мы просто знаем, что он будет автоматически
  // включен до конца. Вот полный пример.


  type Executable[T] = ExecutionContext ?=> T

  def f1(n: Int): Executable[Future[Int]] = f2(n + 1)
  def f2(n: Int): Executable[Future[Int]] = f3(n + 1)
  def f3(n: Int): Executable[Future[Int]] = f4(n + 1)
  def f4(n: Int): Executable[Future[Int]] = {
    val ex = summon[ExecutionContext]
    Future {
      println(s"Hi from the future! n is $n")
      n
    }
  }

  @main def scala3scala3Contextfunctions()={
    given ec: ExecutionContext = scala.concurrent.ExecutionContext.global
    Await.result(f1(10), 1 second)
  }

}



object scala2Contextfunctions {
  def f1(n: Int)(implicit context: ExecutionContext)  = f2(n + 1)
  def f2(n: Int)(implicit context: ExecutionContext) = f3(n + 1)
  def f3(n: Int)(implicit context: ExecutionContext) = f4(n + 1)
  def f4(n: Int)(implicit context: ExecutionContext): Future[Int] = {
      Future {
      println(s"Hi from the future! n is $n")
      n
    }
  }

  @main def scala2scala3Contextfunctions()={
    import concurrent.ExecutionContext.Implicits.global
    Await.result(f1(10), 1 second)
  }

}
*/