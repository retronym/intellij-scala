package p1 {
  class packagealias(s: String)
  package oldname {
    object `package`
    package nested {
      class C
    }
  }

  package newname {
    @packagealias("pl.oldname")
    object `package`
  }

  object Client {
    new p1.newname.nested./*line: 6*/C
  }
}
