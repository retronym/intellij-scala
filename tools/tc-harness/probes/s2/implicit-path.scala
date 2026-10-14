trait Codec { type T; def enc(t: T): String }
object IntCodec extends Codec { type T = Int; def enc(t: Int) = t.toString }
object IP {
  implicit val ic: IntCodec.type = IntCodec
  def enc(c: Codec)(t: c.T): String = c.enc(t)
  val s = enc(IntCodec)(1)
  def encI(t: Int)(implicit c: Codec { type T = Int }) = c.enc(t)
  val s2 = encI(2)
  trait Show[A] { def show(a: A): String }
  implicit def showPath(implicit c: Codec { type T = Int }): Show[c.T] = c.enc(_)
  val s3 = implicitly[Show[Int]].show(3)
}
