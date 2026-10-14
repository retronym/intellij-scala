//> tc --scala 3.3
case class Cfg(n: Int)
object CF:
  type WithCfg[T] = Cfg ?=> T
  def n: WithCfg[Int] = summon[Cfg].n
  def run[T](t: WithCfg[T]): T = t(using Cfg(3))
  val r = run(n + 1)
// ? T r
