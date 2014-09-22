/**
 * Created by fatu on 2014/9/18.
 */
object Main extends App{
  val parser = new ExprParser
  var result = parser.parseAll(parser.expr, "3-4*5")
  if (result.successful) println(result.get)

}
