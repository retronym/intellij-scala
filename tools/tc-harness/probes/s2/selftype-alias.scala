trait Ctx { type T; def mk: T }
trait UsesCtx { self: Ctx =>
  type Alias = T
  def twice: (Alias, T) = (mk, mk)
  def back(a: Alias): T = a
}
object Impl extends Ctx with UsesCtx { type T = Int; def mk = 1 }
object SA {
  val t: (Int, Int) = Impl.twice
  val i: Int = Impl.back(1)
  val x: Impl.Alias = 3
}
