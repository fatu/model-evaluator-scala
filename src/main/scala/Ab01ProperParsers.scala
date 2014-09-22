import scala.util.parsing.combinator.Parsers

/**
 * Created by fatu on 2014/9/19.
 */
trait Ab01ProperParsers extends Parsers {
  type Elem = Char

  val ab01:Parser[Char] = elem('a') | elem('b') | elem('0') | elem('1')

  val myParser: Parser[List[Char]] = ab01*

}
