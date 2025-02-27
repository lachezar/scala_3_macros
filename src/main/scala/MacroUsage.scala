import MacroExample.*

object MacroUsage {
  // does not work: inline val compute: Int = 3 + 4

  val firstUsage = firstMacro(3 + 5, "hello")
  val optDesc = pmOptions(Some(42))
  val optDesc2 = pmOptions(Option(42).map(_ * 2))

  final case class Test(x: Int) {
    def magic(y: Int): String = s" called with $x and $y"
  }

  val x: Int = 3
  val res: String = callMethod(Test(42), "magic", x)
  // val res2: String = callMethod(Test(42), "magic2", x) does not compile
}
