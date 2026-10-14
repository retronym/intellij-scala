object Outer { object Mid { class C; val c = new C } ; val m: Mid.type = Mid }
object CP {
  val a: Outer.Mid.C = Outer.Mid.c
  val b: Outer.m.C = Outer.Mid.c
  val x: Outer.Mid.C = b
  type Alias = Outer.Mid.type
  val al: Alias = Outer.m
  val c: al.C = a
  def f(m: Outer.Mid.type): m.C = m.c
  val d: Outer.Mid.C = f(Outer.m)
}
