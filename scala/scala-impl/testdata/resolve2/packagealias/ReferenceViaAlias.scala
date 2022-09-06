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
    new p1.newname./*line: 5*/C
  }
}
