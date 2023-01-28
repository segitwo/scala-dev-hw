package module2

import scala.language.implicitConversions

object higher_kinded_types extends App{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}




  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  abstract class Conv[F[_]] {
    def conv[A](fa: F[A]): Bindable[F, A]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = {
    fa.flatMap(a => fb.map(b => (a, b)))
  }


  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](lst: List[A]): Bindable[List, A] = new Bindable[List, A] {
    override def map[B](f: A => B): List[B] = lst.map(f)

    override def flatMap[B](f: A => List[B]): List[B] = lst.flatMap(f)
  }

  implicit val optBindableConv: Conv[Option] = new Conv[Option] {
    override def conv[A](fa: Option[A]): Bindable[Option, A] = optBindable(fa)
  }

  implicit val listBindableConv: Conv[List] = new Conv[List] {
    override def conv[A](fa: List[A]): Bindable[List, A] = listBindable(fa)
  }

  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit con: Conv[F]): F[(A, B)] =
    tupleBindable(con.conv(fa), con.conv(fb))

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

//  val r3 = println(tupleBindable(optBindable(optA), optBindable(optB)))
 // val r4 = ???

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))

}