import scala.util.parsing.input.Position

trait Expression[A] {

}

object Expression {  
  implicit object ANFExpExpression extends Expression[ANFExp]
  implicit object SchemeExpExpression extends Expression[SchemeExp]
}
