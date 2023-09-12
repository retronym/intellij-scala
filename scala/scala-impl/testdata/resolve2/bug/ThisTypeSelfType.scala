trait FOGO {
  def foo = 1
}

class OGO {
  self: FOGO =>

//  def x(__DEBUG__: Any): this.type = this

  val y: this.type = ??? // = x("")
  y./* line: 2 */foo


}