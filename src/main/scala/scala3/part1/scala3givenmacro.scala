package part1.scala3givenmacro
/*
object scala3givenmacro {

  trait Show[T] {
    inline def show(x: T): String
  }

  case class Foo(x: Int)

  inline given Show[Foo] with {
    /*transparent*/ inline def show(x: Foo): String = s"11 ${ x.toString } 111"
  }

  @main def scala3givenmacrostart()={
    println(summon[Show[Foo]].show(Foo(7)))
  }

}*/