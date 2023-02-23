package module3

import zio.ZIO
import zio.console.{getStrLn, putStrLn}
import zio.duration.durationInt
import zio.random.Random
import zio.test.Assertion.{anything, equalTo, hasSize, isSubtype, throws}
import zio.test.{DefaultRunnableSpec, ZSpec}
import zio.test._
import zio.test.environment._
import zio.test.TestAspect._

import scala.language.postfixOps
import java.io.IOException


object BasicZIOSpec extends DefaultRunnableSpec{

  val greeter: ZIO[zio.console.Console, IOException, Unit] = for{
    _ <- putStrLn("Как тебя зовут")
    name <- getStrLn
    _ <- putStrLn(s"Привет, $name")
    age <- getStrLn
    _ <- putStrLn(s"Age $age")
  } yield ()


  val app = ZIO.sleep(2 seconds) *> zio.console.putStrLn("Hello")


  val intGen: Gen[Random, Int] = Gen.anyInt

  override def spec = suite("Basic")(
    suite("Arithmetic")(
      test("2*2")(
        assert(2 * 2)(equalTo(4))
      ),
      test("division by zero")(
        assert(2 / 0)(throws(isSubtype[ArithmeticException](anything)))
      )
    ),
    suite("Effect test")(
      testM("simple effect")(
        assertM(ZIO.succeed(2 * 2))(equalTo(4))
      ),
      testM("int addition is associative"){
        check(intGen, intGen, intGen){ (x, y, z) =>
          val left = (x + y) + z
          val right = x + (y + z)
          assert(left)(equalTo(right))
        }
      },
      testM("testConsole")(
        for{
          _ <- TestConsole.feedLines("Alex", "18")
          _ <- greeter
          value <- TestConsole.output
        } yield {
          assert(value(1))(equalTo("Привет, Alex\n"))
        }
      ),
      testM("test clock")(
        for{
          fiber <- app.fork
          _ <- TestClock.adjust(2 seconds)
          _ <- fiber.join
          value <- TestConsole.output
        } yield {
          assert(value(0))(equalTo("Hello\n"))
        }
      ),
      testM("counter")(
        assertM(zioDS.ref.updateCounter)(equalTo(5))
      )@@flaky
    )
  )
}
