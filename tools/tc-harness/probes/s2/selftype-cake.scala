trait Comp { self: Reg with Log =>
  def run(): String = reg.lookup("x") + log.prefix
}
trait Reg { val reg: Registry; class Registry { def lookup(k: String): String = k } }
trait Log { val log: Logger; class Logger { val prefix = ">" } }
object App extends Comp with Reg with Log {
  val reg = new Registry
  val log = new Logger
  val r = run()
  val r2: App.reg.type = reg
}
