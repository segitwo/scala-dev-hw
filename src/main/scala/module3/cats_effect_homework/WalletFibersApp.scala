package module3.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.{Duration, DurationInt}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def launchToPup(wallet: Wallet[IO], duration: Duration, amount: BigDecimal): IO[Unit] =
    IO.sleep(duration) *> wallet.topup(amount)

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      f1 <- launchToPup(wallet1, 100.millis, 100.0).iterateWhile(_ => true).start
      f2 <- launchToPup(wallet2, 500.millis, 100.0).iterateWhile(_ => true).start
      f3 <- launchToPup(wallet3, 2000.millis, 100.0).iterateWhile(_ => true).start
      f4 <- (IO.sleep(2.second)
        *> wallet1.balance.map(b => println(s"Wallet1 balance: $b"))
        *> wallet2.balance.map(b => println(s"Wallet2 balance: $b"))
        *> wallet3.balance.map(b => println(s"Wallet3 balance: $b")))
        .iterateWhile(_ => true).start
      _ <- IO.readLine
      _ <- f1.cancel *> f2.cancel *> f3.cancel *> f4.cancel
    } yield ()

}
