package module3

import module3.functional_effects.functionalProgram.declarativeEncoding.Console.succeed

import scala.io.StdIn

object functional_effects {


  object simpleProgram {

    val greet = {
      println("Как тебя зовут?")
      val name = StdIn.readLine()
      println(s"Привет, $name")
    }

    val askForAge = {
      println("Сколько тебе лет?")
      val age = StdIn.readInt()
      if (age > 18) println("Можешь проходить")
      else println("Ты еще не можешь пройти")
    }


    def greetAndAskForAge = ???


  }


  object functionalProgram {

    /**
     * Executable encoding and Declarative encoding
     */

    object executableEncoding {

      /**
       * 1. Объявить исполняемую модель Console
       */

      case class Console[A](run: () => A){ self =>

        def flatMap[B](f: A => Console[B]): Console[B] =
          Console.succeed(f(self.run()).run())
        def map[B](f: A => B): Console[B] =
          flatMap(a => Console.succeed(f(a)))
      }

      object Console{
        def succeed[A](a: => A): Console[A] = Console(() => a)
        def printLine(str: String): Console[Unit] = Console(() => println(str))
        def readLine(): Console[String] = Console(() => StdIn.readLine())
      }

//      val greet = {
//        println("Как тебя зовут?")
//        val name = StdIn.readLine()
//        println(s"Привет, $name")
//      }

      val p1: Console[Unit] = for {
        _ <- Console.printLine("Как тебя зовут?")
        name <- Console.readLine()
        _ <- Console.printLine(s"Привет, $name")
      } yield ()

      val p2 = for {
        _<- Console.printLine("Сколько тебе лет?")
        age <- Console.readLine()
        _ <- if (age.toInt >= 18) Console.printLine("Можешь проходить") else Console.printLine("Ты еще не можешь пройти")
      } yield ()


      val p3 = p1.flatMap(_ => p2)

      /**
       * 2. Объявить конструкторы
       */


      /**
       * 3. Описать желаемую программу с помощью нашей модель
       */

    }


    object declarativeEncoding {

      /**
       * 1. Объявить декларативную модель Console
       */

      sealed trait Console[A] { self =>

        def flatMap[B](f: A => Console[B]): Console[B] = FlatMap(self, f)

        def map[B](f: A => B): Console[B] = {
          val ff: A => Console[B] = a => succeed(f(a))
          FlatMap(self, ff)
        }

      }
      case class Succeed[A](v: () => A) extends Console[A]
      case class PrintLine[A](str: String, rest: Console[A]) extends Console[A]
      case class ReadLine[A](str: String => Console[A]) extends Console[A]
      case class FlatMap[A, B](a: Console[A], f: A => Console[B]) extends Console[B]
      case class Zip[A, B](a: Console[A], b: Console[B]) extends Console[(A, B)]


      object Console{

        def succeed[A](v: => A): Console[A] = Succeed(() => v)
        def printLine(str: String): Console[Unit] = PrintLine(str, succeed())
        def readLine: Console[String] = ReadLine(str => succeed(str))




      }

      //      val greet = {
      //        println("Как тебя зовут?")
      //        val name = StdIn.readLine()
      //        println(s"Привет, $name")
      //      }

      val p1: Console[Unit] = PrintLine("Как тебя зовут?",
        ReadLine(name =>
          PrintLine(s"Привет, $name", Succeed(() => ())))
      )

      val p2 = for{
        _ <- Console.printLine("Как тебя зовут?")
        name <- Console.readLine
        _ <- Console.printLine(s"Привет, $name")
      } yield ()

      val p3 = for {
        _<- Console.printLine("Сколько тебе лет?")
        age <- Console.readLine
        _ <- if (age.toInt >= 18) Console.printLine("Можешь проходить") else Console.printLine("Ты еще не можешь пройти")
      } yield ()


      val p4 = p2.flatMap(_ => p3)


      def interpret[A, B](console: Console[A]): A = console match {
        case Succeed(v) => v()
        case PrintLine(str, rest) =>
          println(str)
          interpret(rest)
        case ReadLine(f) =>
          interpret(f(StdIn.readLine()))
        case FlatMap(a: Console[A], ff: (A => Console[B])) =>
          interpret(ff(interpret(a)))
      }
      def interpretString = ???






      /**
       * 2. Написать конструкторы
       * 
       */


      /**
       * 3. Описать желаемую программу с помощью нашей модели
       */



      /**
       * 4. Написать операторы
       *
       */


      /**
       * 5. Написать интерпретатор для нашей ф-циональной модели
       *
       */



      /**
       * Реализуем туже прошрамму что и в случае с исполняемым подходом
       */

    }

  }

}