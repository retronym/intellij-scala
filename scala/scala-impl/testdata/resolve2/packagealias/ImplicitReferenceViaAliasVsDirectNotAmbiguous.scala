package p1 {
  class packagealias(s: String)
  package oldname {
    object `package`
    class C
  }

  package newname {
    @packagealias("pl.oldname")
    object `package`
  }

  object Client {
    import p1.oldname._
    import p1.newname._
    new /*line: 5*/C
  }
}
