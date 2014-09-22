/**
 * Created by fatu on 2014/9/19.
 */
sealed abstract class Result
case class Success(result: String) extends Result
case class Failure(message: String) extends Result
