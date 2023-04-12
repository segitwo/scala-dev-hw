package part2.scala3TypeMatch
/*
object scala3TypeMatch {

  //match type сводится к одной из его правых частей, в зависимости от типа его проверяемого. Например:

  type FirstComponentOf[T] = T match
  case String      => Option[Char]
  case Int         => Int
  case Iterable[t] => Option[t]

  // recursive
  type Node[T] = T match
  case Iterable[t] => Node[t]
  case Array[t]    => Node[t]
  case AnyVal      => T

  //Благодаря типам соответствия мы можем написать метод,
  // возвращающий первый элемент заданного типа без дублирования кода:
  def firstComponentOf[U](elem: U): FirstComponentOf[U] = elem match
  case s: String => if (s.nonEmpty) Some(s.charAt(0)) else Option.empty[Char]
  case i: Int    => i.abs.toString.charAt(0).asDigit
  case it: Iterable[_] => it.headOption


  @main def scala3TypeMatchEx()={
    val aNumber: FirstComponentOf[Int] = 2
    val aChar: FirstComponentOf[String] = Some('b')
    val anItem: FirstComponentOf[Seq[Float]] = Some(3.2f)

  }
}
*/