package gsoc
package contributors

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.frp.given
import calico.syntax.*
import org.scalajs.dom
import scala.concurrent.duration.DurationInt

private case class Planet(
    name: String,
    radius: Double,
    period: Double,
    color: String,
    symbol: String)

private case class TransferParams(
    semiMajor: Double,
    dv1: Double,
    dv2: Double,
    transferDays: Double)

private case class SimState(
    originIdx: Int,
    destIdx: Int,
    progress: Double,
    active: Boolean,
    params: Option[TransferParams],
    zoom: Double,
    launchAngle: Double
)

val abh80: Contributor = Contributor("abh80"):

  val planets = List(
    Planet("Mercury", 0.387, 0.241, "#b0b0b0", "☿"),
    Planet("Venus", 0.723, 0.615, "#e8cda0", "♀"),
    Planet("Earth", 1.0, 1.0, "#4fc3f7", "⊕"),
    Planet("Mars", 1.524, 1.881, "#e57373", "♂"),
    Planet("Jupiter", 5.203, 11.86, "#ffcc80", "♃"),
    Planet("Saturn", 9.537, 29.46, "#fff59d", "♄")
  )

  val muSun = 1.32712440018e20
  val au = 1.496e11

  def computeTransfer(r1AU: Double, r2AU: Double): TransferParams =
    val r1 = r1AU * au
    val r2 = r2AU * au
    val at = (r1 + r2) / 2.0
    val vCirc1 = math.sqrt(muSun / r1)
    val vCirc2 = math.sqrt(muSun / r2)
    val vTransDepart = math.sqrt(muSun * (2.0 / r1 - 1.0 / at))
    val vTransArrive = math.sqrt(muSun * (2.0 / r2 - 1.0 / at))
    val dv1 = math.abs(vTransDepart - vCirc1) / 1000.0
    val dv2 = math.abs(vCirc2 - vTransArrive) / 1000.0
    val transferSec = math.Pi * math.sqrt(at * at * at / muSun)
    TransferParams(at / au, dv1, dv2, transferSec / 86400.0)

  def defaultAngle(idx: Int): Double =
    idx.toDouble * 2.0 * math.Pi / planets.size + math.Pi / 2.0

  def planetDisplayAngle(pIdx: Int, s: SimState): Double =
    s.params match
      case None => defaultAngle(pIdx)
      case Some(tp) =>
        val tYears = tp.transferDays / 365.25
        val p = planets(pIdx)
        if pIdx == s.originIdx then
          s.launchAngle + 2.0 * math.Pi * tYears * s.progress / p.period
        else if pIdx == s.destIdx then
          val destStart = (s.launchAngle + math.Pi) - 2.0 * math.Pi * tYears / p.period
          destStart + 2.0 * math.Pi * tYears * s.progress / p.period
        else defaultAngle(pIdx)

  def ellipsePoint(
      phi: Double,
      a: Double,
      b: Double,
      c: Double,
      alpha: Double,
      scale: Double,
      center: Double
  ): (Double, Double) =
    val lx = a * math.cos(phi) - c
    val ly = b * math.sin(phi)
    val sx = lx * math.cos(alpha) - ly * math.sin(alpha)
    val sy = lx * math.sin(alpha) + ly * math.cos(alpha)
    (center + sx * scale, center - sy * scale)

  val svgSize = 500
  val ctr = 250.0
  val defaultZoom = 10.5
  val minZoom = 0.3
  val maxZoom = 12.0

  def zoomForTransfer(r1AU: Double, r2AU: Double): Double =
    math.max(r1AU, r2AU) * 1.35

  def renderSvg(
      s: SimState,
      stateRef: SignallingRef[IO, SimState]): Resource[IO, HtmlElement[IO]] =
    val pxPerAU = (svgSize / 2.0 - 20) / s.zoom
    div(
      styleAttr := s"width:${svgSize}px;height:${svgSize}px;margin:8px auto;"
    ).flatMap { container =>
      Resource
        .eval(IO {
          val raw = container.asInstanceOf[dom.HTMLElement]
          val svgNS = "http://www.w3.org/2000/svg"
          val svg = dom.document.createElementNS(svgNS, "svg")
          svg.setAttribute("viewBox", s"0 0 $svgSize $svgSize")
          svg.setAttribute("width", s"$svgSize")
          svg.setAttribute("height", s"$svgSize")
          svg.setAttribute(
            "style",
            "background:#0f172a;border-radius:8px;border:1px solid #1e293b;cursor:grab;")

          svg.addEventListener(
            "wheel",
            (evt: dom.Event) => {
              evt.preventDefault()
              val we = evt.asInstanceOf[dom.WheelEvent]
              val factor = if we.deltaY > 0 then 1.15 else 1.0 / 1.15
              stateRef
                .update(st =>
                  st.copy(zoom = math.max(minZoom, math.min(maxZoom, st.zoom * factor))))
                .unsafeRunAndForget()
            }
          )

          val zoomRatio = s.zoom / defaultZoom
          val dotR = math.max(2.0, math.min(5.0, 3.0 * zoomRatio))
          val fontSize = math.max(7.0, math.min(12.0, 9.0 * zoomRatio))
          val strokeW = math.max(0.3, math.min(1.0, 0.5 * zoomRatio))
          val sunR = math.max(3.0, math.min(8.0, 6.0 * zoomRatio))

          val sun = dom.document.createElementNS(svgNS, "circle")
          sun.setAttribute("cx", s"$ctr")
          sun.setAttribute("cy", s"$ctr")
          sun.setAttribute("r", s"$sunR")
          sun.setAttribute("fill", "#fbbf24")
          svg.appendChild(sun)

          planets.zipWithIndex.foreach { (p, i) =>
            val rPx = p.radius * pxPerAU
            val orbit = dom.document.createElementNS(svgNS, "circle")
            orbit.setAttribute("cx", s"$ctr")
            orbit.setAttribute("cy", s"$ctr")
            orbit.setAttribute("r", s"$rPx")
            orbit.setAttribute("fill", "none")
            orbit.setAttribute("stroke", "#1e293b")
            orbit.setAttribute("stroke-width", s"$strokeW")
            svg.appendChild(orbit)

            val angle = planetDisplayAngle(i, s)
            val px = ctr + rPx * math.cos(angle)
            val py = ctr - rPx * math.sin(angle)

            val dot = dom.document.createElementNS(svgNS, "circle")
            dot.setAttribute("cx", s"$px")
            dot.setAttribute("cy", s"$py")
            dot.setAttribute("r", s"$dotR")
            dot.setAttribute("fill", p.color)
            svg.appendChild(dot)

            val label = dom.document.createElementNS(svgNS, "text")
            label.setAttribute("x", s"${px + dotR + 3}")
            label.setAttribute("y", s"${py + 3}")
            label.setAttribute("fill", p.color)
            label.setAttribute("font-size", s"$fontSize")
            label.textContent = s"${p.symbol} ${p.name}"
            svg.appendChild(label)
          }

          s.params.foreach { tp =>
            val r1AU = planets(s.originIdx).radius
            val r2AU = planets(s.destIdx).radius
            val outward = r2AU > r1AU
            val rInnerAU = math.min(r1AU, r2AU)
            val rOuterAU = math.max(r1AU, r2AU)
            val aAU = (rInnerAU + rOuterAU) / 2.0
            val cAU = (rOuterAU - rInnerAU) / 2.0
            val bAU = math.sqrt(aAU * aAU - cAU * cAU)
            val alpha = if outward then s.launchAngle else s.launchAngle - math.Pi

            val nSegs = 60
            val (phiStart, phiEnd) =
              if outward then (0.0, math.Pi) else (math.Pi, 2.0 * math.Pi)
            val (dimStart, dimEnd) =
              if outward then (math.Pi, 2.0 * math.Pi) else (0.0, math.Pi)
            val dimD = new StringBuilder("M ")
            for i <- 0 to nSegs do

              val phi = dimStart + (dimEnd - dimStart) * i.toDouble / nSegs

              val (ex, ey) = ellipsePoint(phi, aAU, bAU, cAU, alpha, pxPerAU, ctr)

              if i == 0 then dimD.append(s"$ex $ey")
              else dimD.append(s" L $ex $ey")

            val dimPath = dom.document.createElementNS(svgNS, "path")
            dimPath.setAttribute("d", dimD.toString)
            dimPath.setAttribute("fill", "none")
            dimPath.setAttribute("stroke", "#50688a")
            dimPath.setAttribute("stroke-width", "1")
            dimPath.setAttribute("stroke-dasharray", "3 4")
            dimPath.setAttribute("opacity", "0.5")
            svg.appendChild(dimPath)
            val pathD = new StringBuilder("M ")
            for i <- 0 to nSegs do
              val phi = phiStart + (phiEnd - phiStart) * i.toDouble / nSegs
              val (ex, ey) = ellipsePoint(phi, aAU, bAU, cAU, alpha, pxPerAU, ctr)
              if i == 0 then pathD.append(s"$ex $ey")
              else pathD.append(s" L $ex $ey")

            val path = dom.document.createElementNS(svgNS, "path")
            path.setAttribute("d", pathD.toString)
            path.setAttribute("fill", "none")
            path.setAttribute("stroke", "#60a5fa")
            path.setAttribute("stroke-width", "1.5")
            path.setAttribute("stroke-dasharray", "4 3")
            path.setAttribute("opacity", "0.7")
            svg.appendChild(path)

            val (depX, depY) = ellipsePoint(phiStart, aAU, bAU, cAU, alpha, pxPerAU, ctr)
            val depDot = dom.document.createElementNS(svgNS, "circle")
            depDot.setAttribute("cx", s"$depX")
            depDot.setAttribute("cy", s"$depY")
            depDot.setAttribute("r", "5")
            depDot.setAttribute("fill", "#22c55e")
            depDot.setAttribute("opacity", "0.8")
            svg.appendChild(depDot)

            val (arrX, arrY) = ellipsePoint(phiEnd, aAU, bAU, cAU, alpha, pxPerAU, ctr)
            val arrDot = dom.document.createElementNS(svgNS, "circle")
            arrDot.setAttribute("cx", s"$arrX")
            arrDot.setAttribute("cy", s"$arrY")
            arrDot.setAttribute("r", "5")
            arrDot.setAttribute("fill", "#ef4444")
            arrDot.setAttribute("opacity", "0.8")
            svg.appendChild(arrDot)

            if s.active || s.progress >= 1.0 then
              val t = math.min(s.progress, 1.0)
              val phi = phiStart + (phiEnd - phiStart) * t
              val (sx, sy) = ellipsePoint(phi, aAU, bAU, cAU, alpha, pxPerAU, ctr)
              val craft = dom.document.createElementNS(svgNS, "circle")
              craft.setAttribute("cx", s"$sx")
              craft.setAttribute("cy", s"$sy")
              craft.setAttribute("r", "5")
              craft.setAttribute("fill", "#f59e0b")
              svg.appendChild(craft)

              val glow = dom.document.createElementNS(svgNS, "circle")
              glow.setAttribute("cx", s"$sx")
              glow.setAttribute("cy", s"$sy")
              glow.setAttribute("r", "10")
              glow.setAttribute("fill", "#f59e0b")
              glow.setAttribute("opacity", "0.25")
              svg.appendChild(glow)
          }

          val zoomText = dom.document.createElementNS(svgNS, "text")
          zoomText.setAttribute("x", "10")
          zoomText.setAttribute("y", "20")
          zoomText.setAttribute("fill", "#475569")
          zoomText.setAttribute("font-size", "11")
          zoomText.textContent = f"${s.zoom}%.1f AU · scroll to zoom"
          svg.appendChild(zoomText)

          s.params.foreach { tp =>
            val elapsed = tp.transferDays * s.progress
            val timeText = dom.document.createElementNS(svgNS, "text")
            timeText.setAttribute("x", "10")
            timeText.setAttribute("y", s"${svgSize - 10}")
            timeText.setAttribute("fill", "#475569")
            timeText.setAttribute("font-size", "11")
            timeText.textContent = f"T + ${elapsed}%.0f / ${tp.transferDays}%.0f days"
            svg.appendChild(timeText)
          }

          raw.appendChild(svg)
        })
        .map(_ => container)
    }

  def fmt(d: Double): String = f"$d%.2f"

  val initial = SimState(2, 3, 0.0, false, None, defaultZoom, 0.0)

  SignallingRef[IO].of(initial).toResource.flatMap { state =>

    val loop: IO[Unit] =
      fs2
        .Stream
        .fixedRate[IO](50.millis)
        .evalMap(_ =>
          state.update { s =>
            if s.active && s.progress < 1.0 then s.copy(progress = s.progress + 0.005)
            else if s.active && s.progress >= 1.0 then s.copy(active = false, progress = 1.0)
            else s
          })
        .compile
        .drain

    loop.background >> div(
      styleAttr := "font-family:'Segoe UI',system-ui,sans-serif;background:#0f172a;color:#e2e8f0;padding:20px;border-radius:12px;max-width:520px;border:1px solid #1e293b;box-shadow:0 8px 24px rgba(0,0,0,0.6);",
      p(
        styleAttr := "margin:0 0 2px 0;font-size:0.85em;color:#94a3b8;",
        "I am ",
        span(styleAttr := "color:#60a5fa;font-weight:700;", "@abh80"),
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      h3(
        styleAttr := "margin:10px 0 6px 0;font-size:1em;color:#60a5fa;letter-spacing:1px;",
        "Hohmann Transfer Orbit Simulator"
      ),
      p(
        styleAttr := "margin:0 0 10px 0;font-size:0.78em;color:#64748b;line-height:1.5;",
        "I have keen interest for astrophysics and decided to add a mini simulation of minimum-energy transfers between planets using real orbital data. ",
        "Select origin & destination, hit Launch, and watch the spacecraft follow the computed Hohmann ellipse. You may also use zoom using scroll wheel/pinch on trackpads."
      ),
      div(
        styleAttr := "display:flex;gap:6px;align-items:center;margin-bottom:10px;flex-wrap:wrap;",
        span(styleAttr := "font-size:0.8em;color:#94a3b8;", "From:"),
        button(
          styleAttr := "padding:4px 12px;border-radius:6px;border:1px solid #334155;background:#1e293b;color:#e2e8f0;font-size:0.82em;cursor:pointer;min-width:80px;",
          onClick --> (_.foreach { _ =>
            state.update { s =>
              val next = (s.originIdx + 1) % planets.size
              val idx = if next == s.destIdx then (next + 1) % planets.size else next
              s.copy(originIdx = idx, active = false, progress = 0.0, params = None)
            }
          }),
          state.map(s => s"${planets(s.originIdx).symbol} ${planets(s.originIdx).name}")
        ),
        span(styleAttr := "font-size:0.9em;color:#475569;", "→"),
        span(styleAttr := "font-size:0.8em;color:#94a3b8;", "To:"),
        button(
          styleAttr := "padding:4px 12px;border-radius:6px;border:1px solid #334155;background:#1e293b;color:#e2e8f0;font-size:0.82em;cursor:pointer;min-width:80px;",
          onClick --> (_.foreach { _ =>
            state.update { s =>
              val next = (s.destIdx + 1) % planets.size
              val idx = if next == s.originIdx then (next + 1) % planets.size else next
              s.copy(destIdx = idx, active = false, progress = 0.0, params = None)
            }
          }),
          state.map(s => s"${planets(s.destIdx).symbol} ${planets(s.destIdx).name}")
        ),
        button(
          styleAttr := "padding:5px 16px;border-radius:6px;border:none;cursor:pointer;font-size:0.83em;background:#2563eb;color:#fff;font-weight:600;",
          onClick --> (_.foreach { _ =>
            state.update { s =>
              val origin = planets(s.originIdx)
              val dest = planets(s.destIdx)
              val tp = computeTransfer(origin.radius, dest.radius)
              val autoZoom = zoomForTransfer(origin.radius, dest.radius)
              val angle = defaultAngle(s.originIdx)
              s.copy(
                params = Some(tp),
                active = true,
                progress = 0.0,
                zoom = autoZoom,
                launchAngle = angle)
            }
          }),
          "Launch"
        ),
        button(
          styleAttr := "padding:5px 10px;border-radius:6px;border:1px solid #334155;cursor:pointer;font-size:0.78em;background:#1e293b;color:#94a3b8;",
          onClick --> (_.foreach { _ => state.update(s => s.copy(zoom = defaultZoom)) }),
          "Reset Zoom"
        )
      ),
      state.map(s => renderSvg(s, state)),
      state.map { s =>
        s.params match
          case Some(tp) =>
            div(
              styleAttr := "margin-top:10px;display:grid;grid-template-columns:1fr 1fr;gap:6px 16px;font-size:0.82em;padding:10px;background:#1e293b;border-radius:8px;border:1px solid #334155;",
              div(
                span(styleAttr := "color:#94a3b8;", "Δv₁: "),
                span(styleAttr := "color:#60a5fa;font-weight:600;", s"${fmt(tp.dv1)} km/s")),
              div(
                span(styleAttr := "color:#94a3b8;", "Δv₂: "),
                span(styleAttr := "color:#60a5fa;font-weight:600;", s"${fmt(tp.dv2)} km/s")),
              div(
                span(styleAttr := "color:#94a3b8;", "Total Δv: "),
                span(
                  styleAttr := "color:#f59e0b;font-weight:600;",
                  s"${fmt(tp.dv1 + tp.dv2)} km/s")),
              div(
                span(styleAttr := "color:#94a3b8;", "Transfer: "),
                span(
                  styleAttr := "color:#e2e8f0;font-weight:600;",
                  s"${fmt(tp.transferDays)} days")),
              div(
                styleAttr := "grid-column:span 2;",
                span(styleAttr := "color:#94a3b8;", "Semi-major axis: "),
                span(styleAttr := "color:#e2e8f0;font-weight:600;", s"${fmt(tp.semiMajor)} AU")
              )
            )
          case None =>
            p(
              styleAttr := "margin-top:10px;font-size:0.8em;color:#475569;font-style:italic;",
              "Select planets and click Launch to compute transfer parameters."
            )
      }
    )
  }
