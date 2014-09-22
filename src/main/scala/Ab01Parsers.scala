import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

/**
 * Created by fatu on 2014/9/19.
 */
trait Ab01Parsers extends Parsers {
  type Elem = Char

  var aParser = new Parser[Char] {
    override def apply(in: Input): ParseResult[Char] = {
      val c = in.first
      if (c == 'a') Success(c, in.rest)
      else Failure("Expected 'a' got '" + c + "'", in)
    }
  }

  def charParser(expected:Char) = new Parser[Char] {
    def apply(in:Input):ParseResult[Char] = {
      val c = in.first
      if (c == expected) Success(c, in.rest)
      else Failure("Expected '" + expected + "' got '" + c + "'", in)
    }
  }


  abstract class Parser[T] extends super.Parser[T] {
    def or(right:Parser[T]):Parser[T] = {
      val left = this
      new Parser[T] {
        def apply(in:Input) =
          left(in) match {
            case s:Success[T] => s
            case _            => right(in)
          }
      }
    }
  }

  val ab01NotNice = charParser('a') or charParser ('b') or charParser('0') or charParser ('1')

  def repeat[T](p: Parser[T]) = new Parser[List[T]] {
    def apply(in: Input): Success[List[T]] = {
      p (in) match {
        case Success(t, next) => val s = apply(next)
          Success(t::s.get, s.next)
        case _                => Success(Nil, in)
      }
    }
  }

  val myParser = repeat(ab01NotNice)

  def run(s: String): Option[List[Char]] = {
    val input = new CharSequenceReader(s)

    myParser(input) match {
      case Success(list, next) if (next.atEnd) => Some(list)
      case _                                   => None
    }
  }
}
