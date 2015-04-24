package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (x: String,
      expr: Signal[Expr]) =>
        (x, Signal {
          eval(expr(), namedExpressions - x)
        })
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalHelp(a: Expr) = eval(a, references)

    expr match {
      case Literal(v) => v
      case Ref(name) => if (references contains name) eval(references(name)(), references - name) else Double.NaN
      case Plus(a, b) => evalHelp(a) + evalHelp(b)
      case Minus(a, b) => evalHelp(a) - evalHelp(b)
      case Times(a, b) => evalHelp(a) * evalHelp(b)
      case Divide(a, b) => {
        val evalB = evalHelp(b)
        if (evalB == 0) Double.NaN else evalHelp(a) / evalB
      }
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
