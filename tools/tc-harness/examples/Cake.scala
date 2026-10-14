trait Outer {
  type T
  class Inner { def t: T = ??? }
  val inner: Inner = new Inner
  type __q_it = inner.type
}
object Main {
  val o: Outer { type T = Int } = ???
  val x: Int = o.inner.t
  val bad: String = o.inner.t
  type __q_ot = o.T
}
// ? C ot Int
// ? E ot Int
// ? T x
