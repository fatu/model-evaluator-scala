import org.scalatest.{FlatSpecLike, Matchers}

import scala.util.parsing.input.CharSequenceReader

/**
 * Created by fatu on 2014/9/22.
 */
class WoeParsersTest extends WoeParsers with FlatSpecLike with Matchers{

  private def parsing[T](s:String)(implicit p:Parser[T]):T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t,_)     => t
      case NoSuccess(msg,_) => throw new IllegalArgumentException(
        "Could not parse '" + s + "': " + msg)
    }
  }

  private def assertFail[T](input:String)(implicit p:Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  "The ExpressionParsers" should "parse boolean literals" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    implicit val parserToTest = boolean
    parsing("true")  should equal(BooleanLiteral(true))
    parsing("false") should equal(BooleanLiteral(false))
    assertFail("True")
    assertFail("False")
    assertFail("TRUE")
    assertFail("FALSE")
    assertFail("truefoo")
  }

  it should "parse floating point numbers" in {
    implicit val parserToTest = double
    parsing("0.0")     should equal (NumberLiteral(0.0))
    parsing("1.0")     should equal (NumberLiteral(1.0))
    parsing("-1.0")    should equal (NumberLiteral(-1.0))
    parsing("0.2")     should equal (NumberLiteral(0.2))
    parsing("-0.2")    should equal (NumberLiteral(-0.2))
    parsing(".2")      should equal (NumberLiteral(.2))
    parsing("-.2")     should equal (NumberLiteral(-.2))
    parsing("2.0e3")   should equal (NumberLiteral(2000.0))
    parsing("2.0E3")   should equal (NumberLiteral(2000.0))
    parsing("-2.0e3")  should equal (NumberLiteral(-2000.0))
    parsing("-2.0E3")  should equal (NumberLiteral(-2000.0))
    parsing("2.0e-3")  should equal (NumberLiteral(0.002))
    parsing("2.0E-3")  should equal (NumberLiteral(0.002))
    parsing("-2.0e-3") should equal (NumberLiteral(-0.002))
    parsing("-2.0E-3") should equal (NumberLiteral(-0.002))
  }

  it should "parse integral numbers" in {
    implicit val parserToTest = int
    parsing("0")    should equal (NumberLiteral(0))
    parsing("1")    should equal (NumberLiteral(1))
    parsing("-1")   should equal (NumberLiteral(-1))
    parsing("20")   should equal (NumberLiteral(20))
    parsing("-20")  should equal (NumberLiteral(-20))
  }

}
