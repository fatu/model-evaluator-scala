import scala.util.parsing.combinator.{JavaTokenParsers}

/**
 * Created by fatu on 2014/9/19.
 */
class WoeParsers extends JavaTokenParsers {

  def boolean:Parser[Expression] = ("true" | "false")  ^^ {s => new BooleanLiteral(s.toBoolean)}

  def string  :Parser[Expression] = super.stringLiteral ^^ {s => new StringLiteral(s)}

  def double  :Parser[Expression] = (decimalNumber | floatingPointNumber) ^^ {s => new NumberLiteral(s.toDouble)}

  def int     :Parser[Expression] = wholeNumber ^^ {s => new NumberLiteral(s.toInt)}

  def literal :Parser[Expression] = boolean | string | double | int

  def variable:Parser[Expression] = ident ^^ {s => new Variable(s)}

  def expression:Parser[Expression] = literal | variable

}
