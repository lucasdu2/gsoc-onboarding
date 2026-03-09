package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*

val arnvptl: Contributor = Contributor("arnvptl"):
  SignallingRef[IO].of(0).toResource.flatMap { counter =>
    div(
      h2("Click Counter"),
      p(
        "Hi! I'm @arnvptl on GitHub. I agree to follow the Typelevel Code of Conduct and the Typelevel GSoC AI Policy."
      ),
      counter.map(c => p(s"Clicks: $c")),
      button(
        "Click me",
        onClick --> (_.foreach(_ => counter.update(_ + 1)))
      ),
      button(
        "Reset",
        onClick --> (_.foreach(_ => counter.set(0)))
      )
    )
  }
