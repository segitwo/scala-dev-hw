package part1.scala3testInferring
/*
object scala3testInferring {
  def descending[T](using asc: Ord[T]): Ord[T] = new Ord[T]:
    def compare (x:T, y:T) = asc.compare(y,x)

  def minimum[T](xs: List[T])(using Ord[T]) =
    maximum(xs)(using descending)

  @main def scala3testInferringEx()={
    val xs = 10::2::3::Nil
    println(minimum(xs))
  }


}
*/



















/*
import scala.scala3test.maximum

object scala3testInferring {

  def descending[T](using asc: Ord[T]): Ord[T] = new Ord[T]:
    def compare(x: T, y: T) = asc.compare(y, x)

  def minimum[T](xs: List[T])(using Ord[T]) =
    maximum(xs)(using descending)


  @main def scala3testInferringstart()={
    val xs = (1 :: 2 :: 3 :: Nil)
    println(minimum(xs))
  }

}
*/