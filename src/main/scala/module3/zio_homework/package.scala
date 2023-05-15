package module3

import module3.zioConcurrency.printEffectRunningTime
import zio.{Has, Task, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  def readInt: ZIO[Console, Throwable, Int] = getStrLn.flatMap(str => ZIO.effect(str.toInt))

  lazy val readIntOrRetry: ZIO[Console, Throwable, Int] = readInt.orElse(
    putStrLn("Введите число. Вы ввели строку.") *> readIntOrRetry
  )

  lazy val guessProgram: ZIO[Console with Random, Throwable, Unit] = for {
    number <- nextIntBetween(1, 4)
    _ <- putStrLn("Угадайте число от 1 до 3")
    guess <- readIntOrRetry
    _ <- if (guess == number) putStrLn(s"Вы угадали!!!")
         else putStrLn("Попробуйте еще раз") *> guessProgram
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](zio: ZIO[R, E, A])(f: A => Boolean): ZIO[R, E, A] =
      zio.flatMap(a => if (!f(a)) doWhile(zio)(f) else ZIO.succeed(a))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = ???


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = ZIO.sleep(1.second) *> nextIntBetween(0, 11)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)


  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Clock with Console with Random, IOException, Int] = printEffectRunningTime(
    for {
      numbers <- ZIO.collectAll(effects)
      sum = numbers.fold(0)(_ + _)
      _ <- putStrLn(sum.toString)
    } yield sum
  )




  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: ZIO[Clock with Console with Random, IOException, Int] = printEffectRunningTime(
    for {
      numbers <- ZIO.collectAllPar(effects)
      sum = numbers.fold(0)(_ + _)
      _ <- putStrLn(sum.toString)
    } yield sum
  )


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  @accessible
  object PrintEffectRunningTime {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with R, E, A]
    }

    class Impl() extends Service {
          val currentTime: URIO[Clock, Long] = zio.clock.currentTime(TimeUnit.SECONDS)
          def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with R, E, A] = for {
            start <- currentTime
            r <- zio
            end <- currentTime
            _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
          } yield r

    }

     val live = ???
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ???

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = ???

}
