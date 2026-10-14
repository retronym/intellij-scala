//> tc --scala 3.3
object Units:
  opaque type Meters = Double
  object Meters:
    def apply(d: Double): Meters = d
  extension (m: Meters) def value: Double = m
object Use:
  import Units.*
  val m = Meters(1.0)
  val v: Double = m.value
  val bad: Double = m
