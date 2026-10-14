// ff0cb8e32d, 41f85be577: Null, aliases of Null, admitsNull
object NA {
  type N = Null
  type S = String
  type R = AnyRef { def foo: Int }
  val a: S = null
  val b: N = null
  val c: String = b
  val d: R = b
  val e: Int = b
  type Bounded <: AnyRef
  val f: Bounded = null
  type Lower >: Null <: AnyRef
  val g: Lower = null
  val h: Nothing = b
  def gen[T >: Null]: T = null
  val i: List[Int] = gen
}
