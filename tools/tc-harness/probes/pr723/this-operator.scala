// 92d7bcf8ee: `this` as operator receiver / dependent arg is C.this.type
class Vec(val x: Int) {
  def +(o: Vec): Vec = new Vec(x + o.x)
  def twice: Vec = this + this
  def dep(v: Vec)(w: v.type): v.type = w
  def self: this.type = dep(this)(this)
  def ==~(o: Vec): Boolean = x == o.x
  def eqSelf = this ==~ this
}
object TO { val v = new Vec(1); val s: v.type = v.self; val t: Vec = v.twice }
