trait Base { type T; val t: T; object Obj { def get: T = t }; class C(val c: T) }
object Impl extends Base { type T = String; val t = "s" }
class Impl2 extends Base { type T = Int; val t = 1 }
object MA {
  val g: String = Impl.Obj.get
  val c = new Impl.C("x")
  val cc: String = c.c
  val i2 = new Impl2
  val g2: Int = i2.Obj.get
  val o: i2.Obj.type = i2.Obj
  val c2: Int = new i2.C(3).c
  val bad: String = i2.Obj.get
}
