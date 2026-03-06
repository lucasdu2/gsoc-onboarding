package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*
import calico.frp.given
import cats.syntax.all.*

enum Ingr:
  case Bun, Cheese, Patty, Tomato, Lettuce, Pickle, Ketchup

// represents an ingredient
case class IngRep(name: String, ascii: String, active: SignallingRef[IO, Boolean])

// initalizes plain burger, only patty and bun
def initBurger: IO[Map[Ingr, IngRep]] =
  for
    bun <- SignallingRef[IO].of(true)
    patty <- SignallingRef[IO].of(true)
    cheese <- SignallingRef[IO].of(false)
    lettuce <- SignallingRef[IO].of(false)
    tomato <- SignallingRef[IO].of(false)
    pickle <- SignallingRef[IO].of(false)
  yield Map(
    Ingr.Bun -> IngRep("bun", "    .--------------------.\n   (______________________)\n", bun),
    Ingr.Patty -> IngRep("patty", "   MMMMMMMMMMMMMMMMMMMMMMMM\n", patty),
    Ingr.Cheese -> IngRep("cheese", "   —v——————v——————v—————v—\n", cheese),
    Ingr.Lettuce -> IngRep("lettuce", "   {_.-‘-.___.-‘-.___.-‘-._}\n", lettuce),
    Ingr.Tomato -> IngRep("tomato", "   \\.___.8._______.8.___./\n", tomato),
    Ingr.Pickle -> IngRep("pickle", "  < >< >< >< >< >< >< >< >< >\n", pickle)
  )

val displayOrder: List[Ingr] = List(
  Ingr.Bun,
  Ingr.Pickle,
  Ingr.Tomato,
  Ingr.Cheese,
  Ingr.Patty,
  Ingr.Lettuce
)

val loladenney: Contributor =
  Contributor("loladenney"):
    Resource.eval(initBurger).flatMap { ingredients =>
      div(
        p(
          "Hello, I'm @loladenney on GitHub and L otherwise (please ignore the github deadname haha).",
          br(()),
          "I agree to follow the Typelevel CoC and GSoC AI policy."
        ),
        p(b("Welcome to L's Burgeria. Make your delicious ASCII burger below")),
        div(
          // buttons
          displayOrder.map { ingr =>
            button(
              ingredients(ingr).name, // button name
              styleAttr <-- ingredients(ingr).active.map { active =>
                if active then "background-color: rgb(204, 255, 153);"
                else "background-color: rgb(255, 102, 102);"
              },
              onClick --> (_.foreach(_ => ingredients(ingr).active.update(b => !b)))
            )
          }
        ),
        div(
          // burger display
          styleAttr := "margin:0",
          pre(
            displayOrder.map { ingr =>
              val rep = ingredients(ingr)
              rep.active.map { isactive => if isactive then rep.ascii else "" }
            },
            ingredients(Ingr.Bun).active.map { active =>
              if active then "    \\____________________/\n" else ""
            }
          )
        )
      )
    }
