package part2.scala3OpaqueTypeAliases
/*
object MyMath:

  opaque type Logarithm = Double

  object Logarithm:

  // Это два способа подняться до логарифмического типа.

  def apply(d: Double): Logarithm = math.log(d)

  def safe(d: Double): Option[Logarithm] =
  if d > 0.0 then Some(math.log(d)) else None

  end Logarithm

  // Методы расширения определяют общедоступные API непрозрачных типов.
  extension (x: Logarithm)
  def toDouble: Double = math.exp(x)
  def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
  def * (y: Logarithm): Logarithm = x + y

  end MyMath

  /*
  Это вводит Logarithm как новый абстрактный тип, который реализован как Double.
   Тот факт, что Logarithm совпадает с Double, известен только в области, в которой
   определен Logarithm, что в приведенном выше примере соответствует объекту MyMath.
   Или, другими словами, внутри области он рассматривается как псевдоним типа, но он
   непрозрачен для внешнего мира, где, как следствие, Logarithm рассматривается
   как абстрактный тип, не имеющий ничего общего с Double.

  Общедоступный API Logarithm состоит из методов apply и safe, определенных в
  объекте-компаньоне. Они преобразуют значения Doubles в значения Logarithm.
  Более того, операция toDouble, которая выполняет обратное преобразование,
  и операции + и * определены как методы расширения над значениями логарифма.
  */



  object scala3OpaqueTypeAliases {
    @main def scala3OpaqueTypeAliasesEx()={
      import MyMath.Logarithm

      val l = Logarithm(1.0)
      val l2 = Logarithm(2.0)
      val l3 = l * l2
      val l4 = l + l2

      // а это вызовет ощибки
      /*
      val d: Double = l       // error: found: Logarithm, required: Double
      val l2: Logarithm = 1.0 // error: found: Double, required: Logarithm
      l * 2                   // error: found: Int(2), required: Logarithm
      l / l2                  // error: `/` is not a member of Logarithm
      */
    }

  }*/