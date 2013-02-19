trait Test {
  trait SettingValue {
    def value: Any = 0
  }

  type Setting <: SettingValue

  // okay
  def foo(x: SettingValue {}) = x./*resolved: true*/value
}
