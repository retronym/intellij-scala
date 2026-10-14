class L1 { class L2 { class L3 { def l1: L1.this.type = L1.this; def l2: L2.this.type = L2.this } ; def mk = new L3 }; def mk = new L2 }
object OC {
  val a = new L1
  val b = a.mk
  val c = b.mk
  val x: a.type = c.l1
  val y: b.type = c.l2
  val z: a.L2 = c.l2
  val w: L1#L2#L3 = c
  val a2 = new L1
  val bad: a2.type = c.l1
}
