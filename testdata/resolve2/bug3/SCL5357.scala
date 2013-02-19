trait Test {
  trait SettingValue {
    def value: Any = 0
  }

  type Setting <: SettingValue

  // can't resolve `value`
  def foo(x: Setting {}) = x./*resolved: true*/value
}
