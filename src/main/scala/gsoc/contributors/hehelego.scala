package gsoc
package contributors

import cats.syntax.all.*
import cats.effect.*
import fs2.concurrent.*
import fs2.dom.{HtmlDivElement, HtmlElement}
import calico.html.io.{*, given}
import calico.syntax.*

// a simple sequence builder game
def SequenceBuilder(): Resource[IO, HtmlDivElement[IO]] =
  SignallingRef[IO].of(List(1)).toResource.flatMap { seq =>
    val addSteps = List(-2, -3, -5, 2, 3, 5)
    val mulSteps = List(2, 3, 5, 7)

    div(
      b("Build your sequence!"),
      p("Add by: "),
      div(
        addSteps.map(x =>
          button(
            x.toString,
            onClick --> (_.foreach(_ => seq.update { s => (x + s.head) :: s }))
          ))),
      p("Multiply by: "),
      div(
        mulSteps.map(x =>
          button(
            x.toString,
            onClick --> (_.foreach(_ => seq.update(s => (x * s.head) :: s)))
          ))),
      ul(
        children <-- seq.map(_.map(x => li(x.toString)).sequence)
      )
    )
  }

val app: Resource[IO, HtmlElement[IO]] = div(
  p(
    "Greetings. I am ",
    span(
      styleAttr := "color: #6042f5; font-weight: bold",
      "@hehelego"
    ),
    " on GitHub.",
    "I agree to follow the Typelevel Code of Conduct and GSoC AI policy."
  ),
  SequenceBuilder()
)

// I agree to follow the Code of Conduct and the GSoC AI Policy of Typelevel
val hehelego: Contributor = Contributor("hehelego")(app)
