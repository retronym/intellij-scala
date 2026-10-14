trait Types { type Type; def mkType: Type }
trait Symbols { type Symbol; def mkSym: Symbol }
trait Infer { self: Types with Symbols =>
  def infer(s: Symbol): Type = mkType
  def both: (Symbol, Type) = (mkSym, mkType)
  class Ctx { def sym: Symbol = mkSym; def owner: Infer.this.type = Infer.this }
}
object Global extends Types with Symbols with Infer { type Type = Int; type Symbol = String; def mkType = 1; def mkSym = "s" }
object CS {
  val t: Int = Global.infer("x")
  val b: (String, Int) = Global.both
  val c = new Global.Ctx
  val s: String = c.sym
  val o: Global.type = c.owner
}
