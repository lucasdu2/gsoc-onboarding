package gsoc
package contributors

import cats.effect.*
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.*
import fs2.dom.*
import calico.*
import calico.html.io.{*, given}
import calico.syntax.*
import calico.unsafe.given

import scala.concurrent.duration.*

val jmesyou: Contributor = Contributor("jmesyou"):
  Stream.fixedRate[IO](75.millis).as(1).scanMonoid.holdOptionResource.flatMap { tick =>
    div(
      tick.discrete.map(t => pre(render(t))).holdOptionResource
    )

  }

def render(t: Option[Int]): String = {
  def blank(s: String): String = s.map(c => if c != '\n' then ' ' else c)
  t match {
    case None => blank(text)
    case Some(t) =>
      (processed.take(t) ++ processed.drop(t).map(blank)).reverse.mkString("\n")
  }
}

lazy val processed = text.split("\n").reverse

val text = """
                            &  &  &
                           &&&&&&&&&  &
                             &&&&&&&&&&&&  &
                         & &&& &&&&&&&&&&&
                           &&& &&|&/&|&&&&&
                           &&&&\_\|&/& &&&&
                         && &&& \/~|
                                 /~|/
                                 \||/
                                 \|_/
                                 _/| _/
                                   /|\ /___/&&&&     &
                                   \|     &&&/   & &
                                     |/~  & &&// &&&
                                   /~\|   & &&
                                     \/~
                     &&                /~
                     &&&&& &\__        /~\|
                       &&&&&&&__\___\___   /~~~
                             :___________./~~~\.___________:
                               \                           /
                               \_________________________/
                               (_)                     (_)
                                                                                                                                                            
+----------------------------------------------------------------------------------------+
| Hello, I'm @jmesyou on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy. |
+----------------------------------------------------------------------------------------+
"""
