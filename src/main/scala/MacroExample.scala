import scala.quoted.*

object MacroExample {

  inline def firstMacro(n: Int, s: String): String =
    ${ firstMacroImpl('n, 's) }

  def firstMacroImpl(nAst: Expr[Int], sAst: Expr[String])(using Quotes): Expr[String] =
    val numVal: Int = nAst.valueOrAbort
    val strVal: String = sAst.valueOrAbort

    val x = if numVal > 3 then strVal.repeat(numVal) else strVal.take(numVal / 2)
    Expr(s"macro result is $x")

  inline def pmOptions(inline o: Option[Int]) =
    ${ pmOptionsImpl('o) }

  def pmOptionsImpl(oAst: Expr[Option[Int]])(using Quotes): Expr[String] =
    oAst match
      case '{ Some(42) }                       => Expr("it was 42")
      case '{ Some($x) }                       => Expr(s"got: ${x.show}")
      case x @ '{ ($o: Option[a]).map[b]($f) } => Expr(s"map on option: ${x.show} with func ${f.show}")
      case _                                   => Expr("something else")

  inline def callMethod[A](instance: A, methodName: String, arg: Int): String =
    ${ callMethodImpl('instance, 'methodName, 'arg) }

  def callMethodImpl[A](instance: Expr[A], methodName: Expr[String], arg: Expr[Int])(using q: Quotes): Expr[String] = {
    import q.reflect.*

    val instanceTerm: Term = instance.asTerm
    val method: Select = Select.unique(instanceTerm, methodName.valueOrAbort)
    val res: Apply = Apply(method, List(arg.asTerm))

    res.asExprOf[String]
  }
}
