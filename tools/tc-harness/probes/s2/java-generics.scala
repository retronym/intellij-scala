//> tc --with JavaBox.java
object JG {
  val b = new JavaBox[String]("s")
  val s: String = b.get()
  val w: java.util.List[_ <: CharSequence] = JavaBox.strings()
  val c: CharSequence = w.get(0)
  val raw = JavaBox.raw()
  val m = JavaBox.max(java.util.Arrays.asList(1, 2, 3))
  val st: String = JavaBox.id[String]("x")
}
