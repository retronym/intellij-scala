final case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State(s => { val (s2, a) = run(s); (s2, f(a)) })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { val (s2, a) = run(s); f(a).run(s2) })
}
object FM {
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  val prog: State[Int, String] = for {
    a <- get[Int]
    _ <- put(a + 1)
    b <- get[Int]
  } yield s"$a,$b"
  val pairs = for { x <- List(1, 2); y <- Option("a") } yield (x, y)
  val filtered = for { x <- List(1, 2) if x > 1; (a, b) <- List((x, "s")) } yield a + b
}
// ? T pairs
// ? T filtered
