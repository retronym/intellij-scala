package p1 {
  class packagealias(s: String)
  package oldname {
    object `package` {
      implicit def foo = 42
    }
    class C
  }

  package newname {
    @packagealias("pl.oldname")
    object `package`
  }

  object Client {
    import p1.oldname._

    p1.newname./*line: 5*/foo
  }
}
