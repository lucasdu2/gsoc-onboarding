package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*

val codebydeepankar: Contributor = Contributor("codebydeepankar"):

  // Mood-based intro: cycle through moods with a button
  val moods = Vector(
    ("🚀", "Ready to build amazing things with Scala!", "#6c5ce7"),
    ("💡", "Exploring functional programming one monad at a time.", "#fdcb6e"),
    ("🎯", "Focused on contributing to open source this summer.", "#00b894"),
    ("🔥", "Passionate about type-safe, elegant code.", "#e17055"),
    ("🌈", "Believing great software is built with great communities.", "#0984e3")
  )

  val skills = Vector("Scala", "Cats Effect", "FS2", "Functional Programming", "Open Source")

  def skillPill(sig: SignallingRef[IO, Int], idx: Int, skill: String) =
    sig.map { activeIdx =>
      val isActive = idx == activeIdx
      val bg =
        if isActive then "linear-gradient(90deg, #f093fb, #f5576c)"
        else "rgba(255,255,255,0.08)"
      val fg = if isActive then "#fff" else "#b0b0b0"
      val transform = if isActive then "scale(1.1)" else "scale(1)"
      span(
        styleAttr := s"padding: 5px 14px; border-radius: 20px; background: $bg; color: $fg; font-size: 0.82em; font-weight: 600; transition: all 0.3s; transform: $transform; display: inline-block;",
        skill
      )
    }

  (for
    revealed <- SignallingRef[IO].of(false)
    moodIdx <- SignallingRef[IO].of(0)
    skillHighlight <- SignallingRef[IO].of(0)
  yield (revealed, moodIdx, skillHighlight)).toResource.flatMap {
    (revealed, moodIdx, skillHighlight) =>

      val cycleMood = moodIdx.update(i => (i + 1) % moods.length)
      val cycleSkill = skillHighlight.update(i => (i + 1) % skills.length)

      div(
        styleAttr := "font-family: 'Segoe UI', system-ui, sans-serif; max-width: 520px; padding: 24px; border-radius: 16px; background: linear-gradient(135deg, #0f0c29, #302b63, #24243e); color: #e0e0e0; box-shadow: 0 8px 32px rgba(0,0,0,0.3);",

        // Header with animated gradient name
        div(
          styleAttr := "text-align: center; margin-bottom: 16px;",
          p(
            styleAttr := "margin: 0; font-size: 1.1em;",
            "Hi, I'm ",
            span(
              styleAttr := "font-weight: 800; font-size: 1.3em; background: linear-gradient(90deg, #f093fb, #f5576c, #4facfe); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;",
              "@codebydeepankar"
            )
          ),
          p(
            styleAttr := "margin: 4px 0 0 0; font-size: 0.85em; color: #aaa;",
            "Developer \u2022 Open Source Enthusiast \u2022 GSoC 2026 Aspirant"
          )
        ),

        // CoC agreement
        p(
          styleAttr := "font-size: 0.85em; color: #b0b0b0; text-align: center; margin: 8px 0 16px 0; padding: 8px; background: rgba(255,255,255,0.05); border-radius: 8px; border-left: 3px solid #4facfe;",
          "I agree to follow the Typelevel Code of Conduct and GSoC AI Policy. \u2705"
        ),

        // Mood display
        div(
          styleAttr := "text-align: center; margin: 12px 0;",
          moodIdx.map { idx =>
            val (emoji, text, color) = moods(idx)
            div(
              styleAttr := s"padding: 12px; border-radius: 10px; background: rgba(255,255,255,0.07); border: 1px solid ${color}44; transition: all 0.3s;",
              span(styleAttr := "font-size: 1.4em;", emoji),
              span(styleAttr := s"margin-left: 8px; color: $color; font-weight: 600;", text)
            )
          }
        ),

        // Mood cycle button
        div(
          styleAttr := "text-align: center; margin: 12px 0;",
          button(
            styleAttr := "background: linear-gradient(90deg, #4facfe, #00f2fe); color: #0f0c29; border: none; padding: 10px 24px; border-radius: 25px; cursor: pointer; font-weight: 700; font-size: 0.9em; letter-spacing: 0.5px;",
            onClick --> (_.foreach(_ => cycleMood)),
            "\uD83C\uDFB2 Change My Mood"
          )
        ),

        // Skills ticker
        div(
          styleAttr := "text-align: center; margin: 16px 0 8px 0;",
          p(
            styleAttr := "margin: 0 0 8px 0; font-size: 0.8em; color: #888; text-transform: uppercase; letter-spacing: 2px;",
            "Tech I Love"
          ),
          div(
            styleAttr := "display: flex; gap: 8px; justify-content: center; flex-wrap: wrap;",
            skillPill(skillHighlight, 0, "Scala"),
            skillPill(skillHighlight, 1, "Cats Effect"),
            skillPill(skillHighlight, 2, "FS2"),
            skillPill(skillHighlight, 3, "Functional Programming"),
            skillPill(skillHighlight, 4, "Open Source")
          )
        ),

        // Highlight skill button
        div(
          styleAttr := "text-align: center; margin: 8px 0 16px 0;",
          button(
            styleAttr := "background: linear-gradient(90deg, #f093fb, #f5576c); color: #fff; border: none; padding: 8px 20px; border-radius: 25px; cursor: pointer; font-weight: 600; font-size: 0.85em;",
            onClick --> (_.foreach(_ => cycleSkill)),
            "\u2728 Next Skill"
          )
        ),

        // Reveal section
        revealed.map { r =>
          if r then
            div(
              styleAttr := "padding: 16px; background: rgba(79, 172, 254, 0.1); border-radius: 12px; border: 1px solid rgba(79, 172, 254, 0.25); margin-top: 8px;",
              p(
                styleAttr := "margin: 0 0 8px 0; font-size: 0.95em; color: #4facfe; font-weight: 700;",
                "\uD83D\uDCE3 About Me"
              ),
              p(
                styleAttr := "margin: 0; font-size: 0.88em; line-height: 1.6; color: #d0d0d0;",
                "I'm Deepankar — a developer who loves building things that matter. ",
                "I'm drawn to functional programming for its elegance and safety. ",
                "This summer, I'm excited to contribute to the Typelevel ecosystem ",
                "and grow as an open-source contributor through GSoC 2026!"
              ),
              div(
                styleAttr := "margin-top: 12px; display: flex; gap: 10px; justify-content: center;",
                span(styleAttr := "font-size: 1.5em;", "\uD83D\uDC31"),
                span(styleAttr := "font-size: 1.5em;", "\u2615"),
                span(styleAttr := "font-size: 1.5em;", "\uD83D\uDCBB"),
                span(styleAttr := "font-size: 1.5em;", "\uD83C\uDF1F")
              )
            )
          else div("")
        },

        // Reveal toggle button
        div(
          styleAttr := "text-align: center; margin-top: 12px;",
          button(
            styleAttr := "background: rgba(255,255,255,0.1); color: #e0e0e0; border: 1px solid rgba(255,255,255,0.2); padding: 8px 20px; border-radius: 25px; cursor: pointer; font-size: 0.85em;",
            onClick --> (_.foreach(_ => revealed.update(!_))),
            revealed.map(r =>
              if r then "\uD83D\uDD3C Hide Details" else "\uD83D\uDC40 Learn More About Me")
          )
        )
      )
  }
