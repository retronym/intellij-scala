object Vr {
  class Box[+A](val a: A) { def put[B >: A](b: B): Box[B] = new Box(b) }
  class Sink[-A] { def take(a: A): Unit = () }
  val bs: Box[Any] = new Box[String]("")
  val sk: Sink[String] = new Sink[Any]
  val p = new Box("s").put(1)
  def fn: (Any => String) => (String => Any) = f => f
  trait Bad[+A] { def f(a: A): Unit }
}
// ? T p
