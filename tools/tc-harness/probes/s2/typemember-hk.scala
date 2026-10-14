trait Monad { type F[_]; def pure[A](a: A): F[A]; def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] }
object OptM extends Monad { type F[A] = Option[A]; def pure[A](a: A) = Some(a); def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa.flatMap(f) }
object TH {
  def twice(m: Monad)(x: m.F[Int]): m.F[Int] = m.flatMap(x)(i => m.pure(i * 2))
  val r: Option[Int] = twice(OptM)(Some(1))
  val r2: OptM.F[Int] = OptM.pure(3)
  def gen[M <: Monad](m: M)(i: Int): m.F[Int] = m.pure(i)
  val g: Option[Int] = gen(OptM)(1)
}
