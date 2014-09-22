/**
 * Created by fatu on 2014/9/22.
 */
sealed trait Marker
case class Foo(i: Int) extends Marker
case class Bar(s: String) extends Marker
case class Baz(d: Double) extends Marker
