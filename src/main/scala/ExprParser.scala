import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by fatu on 2014/9/18.
 */
class ExprParser extends RegexParsers {
  val number = "[0-9]+".r

  def expr: Parser[Int] = term ~ opt(("+" | "-") ~ expr) ^^ {
    case t ~ None => t
    case t ~ Some("+" ~ e) => t + e
    case t ~ Some("-" ~ e) => t - e
  }

  def term: Parser[Int] = factor ~ rep("*" ~> factor) ^^ {
    case f ~ r => f * r.product
  }

  def factor: Parser[Int] = number ^^ {
    _.toInt
  } | "(" ~> expr <~ ")"

}
