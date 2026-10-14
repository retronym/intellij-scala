class Env { class Var { def env: Env.this.type = Env.this }; object Vars { def fresh = new Var } }
object OP {
  val e = new Env
  val v: e.Var = e.Vars.fresh
  val ev: e.type = v.env
  def use(x: Env)(v: x.Var): x.type = v.env
  val u: e.type = use(e)(v)
  val w = new Env
  val bad = use(w)(v)
}
