package part2.scala3CompilerFeedback
/*
//!!!!!!!!Showing Where the Problem Is!!!!!!!!!
// `Order` type class definition, similar to the `Ordering` type class of
// the standard library
trait Order[A] {
  def compare(a1: A, a2: A): Int
}

object Order {
  // Provides an implicit instance of type `Order[List[A]]` under the condition
  // that there is an implicit instance of type `Order[A]`
  implicit def orderList[A](implicit orderA: Order[A]): Order[List[A]] = ???
}

// Sorts a `list` of elements of type `A` with their implicit `order` relation
def sort[A](list: List[A])(implicit order: Order[A]): List[A] = ???

  // A class `Foo`
  class Foo

  //вот это закомментировать
  object  Foo{
    implicit def orderBar: Order[Foo] = ???
  }


  object teestfenback {
    // Let’s try to sort a `List[List[Foo]]`
    sort(List(List(new Foo)))

    //Сообщение об ошибке теперь показывает, как далеко зашел компилятор,
    // связывая неявные определения, и где он в конце концов остановился,
    // потому что не удалось найти неявный аргумент. В нашем случае мы видим,
    // что компилятор попробовал определение orderList, но не нашел неявного
    // Order[Foo]. Итак, мы знаем, что для решения проблемы нам нужно реализовать неявный Order[Foo].
  }


  //Suggesting How to Fix the Problem
  // A class `Bar`
  class Bar

  // An implicit `Order[Bar]`
  // (note that it is _not_ in the `Bar` companion object)
  object Implicits {
    implicit def orderBar: Order[Bar] = ???
  }

  object teestfenback1 {
    //вот это закомментировать
    import  scala2.extendingclasses.Implicits.orderBar
    // Let’s try to sort a `List[Bar]`
    sort(List(new Bar))
  }
*/