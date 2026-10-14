object Lb {
  trait A; trait B; class C extends A with B; class D extends A with B
  val x = if (true) new C else new D
  val y = List(new C, new D)
  val z = if (true) Some(1) else None
  val w = if (true) 1 else 1L
  val v = if (true) List(1) else Vector(1)
  val u = Seq(Left(1), Right("s"))
}
// ? T x
// ? T y
// ? T z
// ? T w
// ? T v
// ? T u
