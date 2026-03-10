package gsoc

import cats.effect.*
import cats.syntax.all.*

import contributors.*

import calico.IOWebApp
import calico.html.io.{*, given}
import calico.router.*
import calico.syntax.*
import calico.unsafe.given

import fs2.concurrent.*
import fs2.dom.HtmlElement

import org.http4s.dom.FetchClientBuilder
import org.http4s.syntax.all.*

object Main extends IOWebApp:
  def render: Resource[IO, HtmlElement[IO]] =
    val handles = allContributors.map(_.handle)
    val client = FetchClientBuilder[IO].create

    Router(window).toResource.flatMap { router =>
      SignallingRef[IO]
        .of[Option[Either[String, ValidationResponse]]](None)
        .toResource
        .flatMap { validated =>

          def contributorUri(handle: String) = uri"" +? ("handle" -> handle)

          def nav = ul(handles.toList.map { handle =>
            li(
              a(
                href := "#",
                onClick(router.navigate(contributorUri(handle))),
                s"@$handle"
              )
            )
          })

          val routes = allContributors.reduceMap {
            case Contributor(handle, component) =>
              Routes.one[IO] {
                case uri if uri.query.params.get("handle").contains(handle) => ()
              } { _ => component }
          }

          val validate: IO[Unit] =
            Validation.validate(handles.toList, client).map(Some(_)).flatMap(validated.set)

          div(
            h1("Typelevel GSoC Onboarding"),
            div(
              cls := "container",
              navTag(
                div(
                  cls <-- validated
                    .map {
                      case None => "empty"
                      case Some(Left(_)) => "error"
                      case Some(Right(ValidationResponse(true, _))) => "valid"
                      case Some(Right(ValidationResponse(false, _))) => "invalid"
                    }
                    .map(_ :: "validation-result" :: Nil),
                  validated.map(_.foldMap(_.fold(identity(_), _.toString)))
                ),
                button(
                  onClick --> (_.foreach(_ => validate)),
                  "Check order"
                ),
                nav
              ),
              mainTag(
                routes.toResource.flatMap(router.dispatch)
              )
            )
          )
        }
    }
