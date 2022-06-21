package p1 {
  class packagealias(s: String)
  package oldname {
    object `package`
    class C
    package p2 {
      class C
    }
    package p3 {
      package p4 {
        class C
        object `package` {
          def foo = ""
        }
      }
    }
  }

  package newname {
    @packagealias("pl.oldname")
    object `package`
    package p3 {
      class Other
    }
  }

  object Client {
    p1.newname.p3.p4./*line: 13*/foo
  }
}
