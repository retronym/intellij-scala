//> tc --scala 3.3
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]
given Functor[[X] =>> Either[String, X]] with
  extension [A](fa: Either[String, A]) def map[B](f: A => B) = fa match
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
object TL:
  type Pair = [X] =>> (X, X)
  val p: Pair[Int] = (1, 2)
  def lift[F[_]: Functor, A](fa: F[A]) = fa.map(_.toString)
  val e = lift[[X] =>> Either[String, X], Int](Right(1))
// ? T e
