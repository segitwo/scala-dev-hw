package module3

import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework.config.AppConfig
import zio.{ExitCode, Has, Task, ULayer, URIO, ZIO, ZLayer}
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


  def loadConfigOrDefault: URIO[Console, AppConfig] =
    for {
      conf <- config.load.orElse(Task.succeed(AppConfig("127.0.0.1", "5432")))
      _ <- putStrLn(conf.toString).orDie
    } yield conf


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

  lazy val app: URIO[Clock with Console with Random, Int] = printEffectRunningTime(
    for {
      numbers <- ZIO.collectAll(effects)
      sum = numbers.sum
      _ <- putStrLn(sum.toString).orDie
    } yield sum
  )




  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[Clock with Console with Random, Int] = printEffectRunningTime(
    for {
      numbers <- ZIO.collectAllPar(effects)
      sum = numbers.sum
      _ <- putStrLn(sum.toString).orDie
    } yield sum
  )


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type LogEffectRunningTimeService = Has[LogEffectRunningTimeService.Service]

  @accessible
  object LogEffectRunningTimeService {
    trait Service {
      def logTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    class ServiceImpl() extends Service {
          val currentTime: URIO[Clock, Long] = zio.clock.currentTime(TimeUnit.SECONDS)
          def logTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
            printEffectRunningTime(zio)
    }

    val live: ULayer[LogEffectRunningTimeService] = ZLayer.succeed(new ServiceImpl)
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: URIO[LogEffectRunningTimeService with Console with Clock with Random, Int] =
    LogEffectRunningTimeService.logTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](LogEffectRunningTimeService.live)
}
