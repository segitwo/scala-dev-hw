package part2.scala3extensions
/*
import scala.Extensionmethods.Circle


// Extension Methods
object Extensionmethods{

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle)
  def circumference: Double = c.radius * math.Pi * 2

  @main def ExtensionmethodsEx(): Unit ={
    val circle = Circle(0, 0, 1)
    println(circle.circumference)
  }

}

//Operators
object Operators{
  extension (x: String)
  def < (y: String): Boolean = x.length < y.length

  @main def OperatorsEx() ={
    println("a" < "bs")
  }
}

//Collective Extensions
/*Иногда требуется определить несколько методов расширения,
 которые используют один и тот же left-hand.
 В этом случае можно "вытянуть" общие параметры в одно расширение и
 заключить все методы в фигурные скобки или отступ. Пример:*/

object CollectiveExtension{
  extension (ss: Seq[String])

  def longestStrings: Seq[String] =
  val maxLength = ss.map(_.length).max
  ss.filter(_.length == maxLength)

  def longestString: String = longestStrings.head

  //Обратите внимание на правую часть longestString: она вызывает
  // longestStrings напрямую, неявно предполагая общее
  // расширенное значение ss в качестве получателя.
  //Коллективные расширения, подобные этим, являются сокращением
  // для индивидуальных расширений, где каждый метод определяется отдельно.
  // Например, первое расширение выше расширяется до:
  /*
    extension (ss: Seq[String])
      def longestStrings: Seq[String] =
        val maxLength = ss.map(_.length).max
        ss.filter(_.length == maxLength)

    extension (ss: Seq[String])
      def longestString: String = ss.longestStrings.head*/


  @main def GenericExtensionEx() ={
    //Аргументы типа, соответствующие параметрам типа метода, передаются как обычно:
    println(List("a", "bb", "ccc").longestString)
  }
}






trait IntOps:
  extension (i: Int) def isZero: Boolean = i == 0

  extension (i: Int) def safeMod(x: Int): Option[Int] =
  // extension method defined in same scope IntOps
  if x.isZero then None
  else Some(i * x )

  object IntOpsEx extends IntOps:
  extension (i: Int) def safeDiv(x: Int): Option[Int] =
  // extension method brought into scope via inheritance from IntOps
  if x.isZero then None
  else Some(i / x)

  trait SafeDiv:
  import IntOpsEx.* // brings safeDiv and safeMod into scope

  extension (i: Int) def divide(d: Int): Option[(Int, Int)] =
  // extension methods imported and thus in scope
  (i.safeDiv(d), i.safeMod(d)) match
  case (Some(d), Some(r)) => Some((d, r))
  case _ => None

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle)
  def circumference: Double = c.radius * math.Pi * 2


  object scala3extensions {
    given ops1: IntOps() with {}

    @main def scala3extensionsstart()={

      // By the second rule, an extension method can be made available by defining a given instance containing it
      println(2.safeMod(2))
    }

  }
*/