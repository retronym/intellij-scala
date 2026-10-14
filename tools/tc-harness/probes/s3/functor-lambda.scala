//> tc --scala 3.3
trait Functor[F[_]]:
  extension [A](fa: F[A]) def fmap[B](f: A => B): F[B]
given eitherF: Functor[[X] =>> Either[String, X]] with
  extension [A](fa: Either[String, A]) def fmap[B](f: A => B): Either[String, B] = fa.map(f)
given listF: Functor[List] with
  extension [A](fa: List[A]) def fmap[B](f: A => B): List[B] = fa.map(f)
object TL3:
  def liftCB[F[_]: Functor, A](fa: F[A]) = fa.fmap(_.toString)
  def liftU[F[_], A](fa: F[A])(using F: Functor[F]) = fa.fmap(_.toString)
  def liftE[F[_], A](fa: F[A])(using F: Functor[F]): F[String] = F.fmap(fa)(_.toString)
  val l1 = liftCB(List(1))
  val e1 = liftCB[[X] =>> Either[String, X], Int](Right(1))
  val e2 = liftU[[X] =>> Either[String, X], Int](Right(1))
  val e3 = liftE[[X] =>> Either[String, X], Int](Right(1))
  val e4: Either[String, String] = e1
// ? T l1
// ? T e1
// ? T e2
// ? T e3
// ? T liftU
