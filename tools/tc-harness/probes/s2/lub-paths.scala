class U { class T }
object LP {
  val u1 = new U; val u2 = new U
  val t = if (true) new u1.T else new u2.T
  val l = List(new u1.T, new u1.T)
  val x: List[u1.T] = l
  val y: U#T = t
  val z: u1.T = t
}
// ? T t
// ? T l
