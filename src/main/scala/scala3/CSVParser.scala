package scala3
/*
trait Ord[T]:
  def compare(x: T, y: T): Int
  extension (x: T)
  def < (y: T) = compare(x, y) < 0
  def > (y: T) = compare(x, y) > 0

class MonadParser[T, Src](private val p: Src => (T, Src)) {
  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      val mn = f(word)
      val res = mn.p(rest)

      //с помощью функции — аргумента метода добавляем его в контекст, видимый всем последующим парсерам по цепочке.
      res
    }
  def map[M](f: T => M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }
  def parse(src: Src): T = p(src)._1
}

object MonadParser {
  def apply[T, Src](f: Src => (T, Src)) =
    new MonadParser[T, Src](f)
}

object TestExecution{

  def main(args: Array[String]): Unit = {
    def StringField =
      MonadParser[String, String] { str =>
        val idx = str.indexOf(";")
        if (idx > -1)
          (str.substring(0, idx), str.substring(idx + 1))
        else
          (str, "")
      }
    def IntField = StringField.map(_.toInt)
    def FloatField = StringField.map(_.toFloat)
    def BooleanField = StringField.map(_.toBoolean)

    case class Car(year: Int, mark: String, model: String, comment: String, price: Float)

    val str = "1997;Ford;E350;ac, abs, moon;3000\n1996; Jeep; Grand Cherokee; MUST SELL! air, moon roof, loaded; 4799"

    val parser =
      for {
        year <- IntField
        mark <- StringField
        model <- StringField
        comment <- StringField
        price <- FloatField
      } yield Car(year, mark, model, comment, price)


    val result = str.split('\n').map(parser.parse)

    result
  }
}*/