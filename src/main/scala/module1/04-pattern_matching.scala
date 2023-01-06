package module1

object pattern_matching{
     // Pattern matching




  /**
   * Матчинг на типы
   */

   val i: Any = ???

   i match {
     case Int => println("Int")
     case v: String => println("String")
     case v: List[String] =>
     case v: List[Int] =>
   }


  /**
   * Структурный матчинг
   */

  sealed trait Animal{
    def name: String
    def age: Int

    def whoIam: String  = this match {
      case Dog(name, age) => s"I'm dog $name"
      case Cat(name, age) => s"I'm cat $name"
    }
  }


  case class Dog(name: String, age: Int) extends Animal
  case class Cat(name: String, age: Int) extends Animal

  /**
   * Матчинг на литерал
   */

  val dog: Animal = ???

  val Bim = "Bim"

  dog match {
    case Dog("Bim", age) =>
    case Cat(name, age) =>
    case _ =>
  }


  /**
   * Матчинг на константу
   */

  dog match {
    case Dog(Bim, age) =>
    case Cat(name, age) =>
    case _ =>
  }
  /**
   * Матчинг с условием (гарды)
   */


  dog match {
    case Dog(_, age) if age > 5 =>
    case Cat(name, age) =>
    case _ =>
  }


  /**
   * "as" паттерн
   */

  def treatCat(cat: Cat) = ???
  def treatDog(dog: Dog) = ???

  def treat(a: Animal): Animal = a match {
    case d @ Dog(_, _) =>  treatDog(d)
    case c @ Cat(_, _) =>  treatCat(c)
  }

  val Hello = "hello"


  val str: String = ???

  str match {
    case Hello =>
  }



  /**
   * Списки
   */






      /**
       * используя паттерн матчинг напечатать имя и возраст
       */



      final case class Employee(name: String, address: Address)
      final case class Address(street: String, number: Int)

      val alex = Employee("Alex", Address("XXX", 221))

      /**
       * воспользовавшись паттерн матчингом напечатать номер из поля адрес
       */



      /**
       * Паттерн матчинг может содержать литералы.
       * Реализовать паттерн матчинг на alex с двумя кейсами.
       * 1. Имя должно соотвествовать Alex
       * 2. Все остальные
       */




      /**
       * Паттерны могут содержать условия. В этом случае case сработает,
       * если и паттерн совпал и условие true.
       * Условия в паттерн матчинге называются гардами.
       */




      /**
       * Реализовать паттерн матчинг на alex с двумя кейсами.
       * 1. Имя должно начинаться с A
       * 2. Все остальные
       */


      /**
       *
       * Мы можем поместить кусок паттерна в переменную использую `as` паттерн,
       * x @ ..., где x это любая переменная.
       * Это переменная может использоваться, как в условии,
       * так и внутри кейса
       */

        trait PaymentMethod
        case object Card extends PaymentMethod
        case object WireTransfer extends PaymentMethod
        case object Cash extends PaymentMethod

        case class Order(paymentMethod: PaymentMethod)

        lazy val order: Order = ???

        lazy val pm: PaymentMethod = ???


        def checkByCard(o: Order) = ???

        def checkOther(o: Order) = ???



      /**
       * Мы можем использовать вертикальную черту `|` для матчинга на альтернативы
       */

       sealed trait A
       case class B(v: Int) extends A
       case class C(v: Int) extends A
       case class D(v: Int) extends A

       val a: A = ???

  a match {
    case B(_) | C(_) =>
    case D(v) =>
  }



}