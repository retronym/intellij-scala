import scala.language.higherKinds
trait Functor[F[_]] { def map[A, B](fa: F[A])(f: A => B): F[B] }
object HK {
  implicit val listF: Functor[List] = new Functor[List] { def map[A, B](fa: List[A])(f: A => B) = fa.map(f) }
  type EitherS[A] = Either[String, A]
  implicit val eitherF: Functor[EitherS] = new Functor[EitherS] { def map[A, B](fa: EitherS[A])(f: A => B) = fa.map(f) }
  def lift[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
  val r1 = lift(List(1))(_ + 1)
  val r2 = lift(Right(1): Either[String, Int])(_ + 1)
}
// ? T r1
// ? T r2
