object S3:
  opaque type Meters = Double
  def m(d: Double): Meters = d
  extension (x: Meters) def plus(y: Meters): Meters = x + y
  val sum = m(1).plus(m(2))
  val wrong: Double = sum
  type __q_u = Int | String
// ? T sum
// ? C u Matchable
