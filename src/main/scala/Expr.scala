/**
 * Created by fatu on 2014/9/18.
 */
class Expr
case class Number(value: Int) extends Expr
case class Operator(op: String, left: Expr, right: Expr) extends Expr
