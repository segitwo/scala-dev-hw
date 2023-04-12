package part2.extendingclasses
/*

//Dependent function types
trait Entry { type Key; val key: Key }

def extractKey(e: Entry): e.Key = e.key          // a dependent method

  val extractor: (e: Entry) => e.Key = extractKey  // a dependent function value
  //В Scala уже есть зависимые методы, т.е. методы, в которых
  // тип результата ссылается на некоторые параметры метода.
  // Метод ExtractKey является примером. Его тип результата, e.Key,
  // ссылается на его параметр e (мы также говорим, что e.Key зависит от e).
  // Но до сих пор не было возможности превратить такие методы в значения функций,
  // чтобы их можно было передавать в качестве параметров другим функциям или
  // возвращать в качестве результатов. Зависимые методы нельзя было превратить
  // в функции просто потому, что не было типа, который мог бы их описать.
  //
  //В Scala 3 теперь это возможно. Тип значения извлечения выше:

  // (e: Entry) => e.Key



  //Polymorphic function types

  //Этот тип описывает значения функций, которые принимают
  // тип A в качестве параметра, затем принимают список типа
  // List[A] и возвращают список того же типа List[A].

  import scala.quoted.Expr

  object scala3PolimorficValues {
    //Например, рассмотрим ситуацию, когда у нас есть тип
    // данных для представления выражений простого языка (состоящего только
    // из переменных и function apllications) в строго типизированном виде:
    enum Expr[A]:
    case Var(name: String)
    // => is syntactic sugar for creating instances of functions.
    // B => A, is equivalent to the type Function1[B,A]
    //Function1[Int,String] = myInt => "my int: "+myInt.toString
    case Apply[A, B](fun: Expr[B => A], arg: Expr[B]) extends Expr[A]

    /*
  enum Color(val rgb: Int):
    case Red   extends Color(0xFF0000)
    case Green extends Color(0x00FF00)
    case Blue  extends Color(0x0000FF)

    это Как показывает пример, вы можете определить значение параметра, используя явное предложение extends.
  */


    //Мы хотели бы предоставить пользователям способ отображать функцию на
    // все непосредственные подвыражения данного Expr. Это требует, чтобы
    // данная функция была полиморфной, поскольку каждое подвыражение может
    // иметь другой тип. Вот как это реализовать с помощью полиморфных типов функций:
    def mapSubexpressions[A](e: Expr[A])(f: [A] => Expr[A] => Expr[A]): Expr[A] =
      e match
    case Expr.Apply(fun, arg) => Expr.Apply(f(fun), f(arg))
    case Expr.Var(n) => Expr.Var(n)



    @main def scala3PolimorficValuesEx()={
      //А вот как использовать эту функцию для переноса каждого подвыражения в заданное выражение с
      // вызовом некоторой функции wrap, определенной как переменная:
      val e0 = Expr.Apply(Expr.Var("f"), Expr.Var("a"))
      val e1 = mapSubexpressions(e0)(
        [A] => (se: Expr[A]) => Expr.Apply(Expr.Var[A => A]("wrap"), se))

      println(e1)// Apply(Apply(Var(wrap),Var(f)),Apply(Var(wrap),Var(a)))

      /*
  e1 = {scala3PolimorficValues$Expr$Apply@835} Apply(Apply(Var(wrap),Var(f)),Apply(Var(wrap),Var(a)))
   fun = {scala3PolimorficValues$Expr$Apply@880} Apply(Var(wrap),Var(f))
    fun = {scala3PolimorficValues$Expr$Var@884} Var(wrap)
    arg = {scala3PolimorficValues$Expr$Var@885} Var(f)
   arg = {scala3PolimorficValues$Expr$Apply@881} Apply(Var(wrap),Var(a))
    fun = {scala3PolimorficValues$Expr$Var@888} Var(wrap)
    arg = {scala3PolimorficValues$Expr$Var@889} Var(a)    * */

    }


  }*/