
import PresentationUtil._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object Introduction extends JSApp {

  import Enumeration._

  val introduction = chapter(
    chapterSlide(
      <.h1("Scala Summer School")
    ),

    noHeaderSlide(
      <.h2("Welcome"),
      <.br,
      <.h3("And thanks for being here")
    ),

    noHeaderSlide(
      <.img(
        ^.alt := "Scala + Fp = Love",
        ^.src := "./img/scala_fp.svg"
      )
    ),

    noHeaderSlide(
      <.h2("The Team")
    ),

    slide(
      "The Team",
      <.img(
        ^.cls := "team-img",
        ^.alt := "Yuval Itzchakov",
        ^.src := "./img/me.jpg"
      ),
      <.p("Yuval Itzchakov / Senior Software Engineer @ Data-Platform")
    ),

    slide(
      "Lectures",
      Enumeration(
        Item.stable("Scala 101"),
        Item.fadeIn("Functional Programming 101"),
        Item.fadeIn("Standard Library"),
        Item.fadeIn("Small workshop project"),
        Item.fadeIn("Type Classes and Incarnations"),
        Item.fadeIn("Side Effect and IO"),
        Item.fadeIn("Big workshop project")
      )
    ),

    slide(
      "How it works",
      Enumeration(
        Item.stable("lectures are split into smaller chapters"),
        Item.fadeIn("exercises are mixed in"),
        Item.fadeIn("question and code exercises")
      )
    ),

    slide(
      "How it works",
      Enumeration(
        Item.fadeIn("Breaks as needed (just ask for it :))"),
      )
    ),

    slide(
      "How it works",
      <.h3("If you have a question ask it directly!")
    ),

    noHeaderSlide(
      <.h3("Feedback")
    ),

    slide(
      "Feedback",
      <.h4("Are the lectures helpful?"),
      <.br,
      <.br,
      <.h4("What would you change?")
    ),

    slide(
      "Feedback",
      <.p("Give direct feedback, or at #tech-tlv-fp-in-scala")
    ),

    slide(
      "Learning environment",
      <.p("This is a safe learning environment")
    ),

    slide(
      "Learning environment",
      Enumeration(
        Item.fadeIn(<.p("There are no stupid questions, just ask what you don't understand")),
        Item.fadeIn(<.p("We are all here to learn (me included)"))
      )
    )
  )

  val exercises = chapter(
    chapterSlide(
      <.h2("Coding Exercises")
    ),

    slide(
      "Coding Exercises",
      <.p("All exercise related code can be found in the exercise-directories.")
    ),

    slide(
      "Coding Exercises",
      <.p("We use sbt to build our code and execute tests."),
      <.br,
      <.img(
        ^.alt := "SBT logo",
        ^.src := "./img/sbt_logo.svg"
      )
    ),

    noHeaderSlide(
      <.h3("The Interactive Build Tool"),
      <.br,
      <.p(
        ^.cls := "fragment fade-in",
        "Formerly: The Simple Build Tool"
      )
    ),

    slide(
      "sbt: simple project",
      bash("""
        my-project/
          # project information (sbt version, plugins, ...)
          project/

          # source code directory
          src/
          # compiled results
          target/

          # build file defining the whole project
          build.sbt
      """)
    ),

    slide(
      "sbt: build file",
      scalaC("""
        // build.sbt
        scalaVersion := "2.12.8"
        name         := "my-project"

        libraryDependencies ++= Seq(...)
      """)
    ),

    slide(
      "sbt: sub-projects",
      bash("""
        my-project/
          project/

          # sub projects defined in the build file
          sub-project-0/
            src/
            target/
          sub-project-1/
           ...

          build.sbt
      """)
    ),

    slide(
      "sbt: build file",
      scalaC("""
        // build.sbt
        val common = Seq(
          scalaVersion := "2.12.8"
        )

        lazy val root = project
          .in(file("."))
          .aggregate(subProject0, ...)

        lazy val subProject0 = project
          .in(file("sub-project-0"))
          .settings(
            common,
            libraryDependencies ++= Seq(...)
          )
        ...
      """)
    ),

    slide(
      "sbt: keep it running",
      <.p("SBT needs a lot of time to start ... so keep it running."),
      <.br,
      bash("""
        cd <sbt project root>

        # opens a cli
        sbt
      """)
    ),

    slide(
      "sbt: commands",
      bash("""
        # show all sub-projects
        sbt> projects

        # change to sub-project
        sbt> project <name of sub-project>

        # compile source code
        sbt> compile

        # compile on file change
        sbt> ~compile
      """)
    ),

    slide(
      "sbt: commands",
      bash("""
        # execute all tests
        sbt> test

        # execute a single test
        sbt> test:testOnly <name of test>
      """)
    ),

    slide(
      "sbt: exercise",
      bash("""
        sbt> project <exercise project>
        sbt> compile <your code>
        sbt> test:testOnly <exercise>
      """)
    ),

    slide(
      "Exercise Structure",
      Enumeration(
        Item.stable(<.p("exercise code in main dir, e.g. `exercise1_scala_101/src/", font(^.color := "red", "main"), "/scala/exercises1/`")),
        Item.fadeIn(<.p("solutions are available in the test dir, e.g. `exercise1_scala_101/src/", font(^.color := "red", "test"), "/scala/exercises1/`"))
      )
    ),

    noHeaderSlide(
      <.h3("That is all about sbt for now")
    )
  )

  val repl = chapter(
    chapterSlide(
      <.h2("REPL")
    ),

    slide(
      "Repl",
      <.p("You can use a Scala REPL (Read-Evaluate-Print-Loop) to try code you see during the lectures.")
    ),

    noHeaderSlide(
      <.h3("Built-In Scala REPL")
    ),

    slide(
      "Scala REPL",
      bash("""
        # just execute Scala
        $>scala
      """),
      scalaC("""
        scala> 1 + 1
        res0: Int = 2

        // exit with
        scala> :q
      """)
    ),

    noHeaderSlide(
      <.h3("Ammonite")
    ),

    slide(
      "Ammonite",
      bash("""
        $> amm
      """),
      scalaC("""
        @ 1 + 1
        res0: Int = 2

        @ exit
      """)
    ),

    noHeaderSlide(
      <.h2("Any questions so far?")
    ),

    noHeaderSlide(
      <.h2("Lets begin!")
    )
  )

  val Show = ScalaComponent
    .builder[Unit]("Slideshow")
    .renderStatic(
      <.div(
        ^.cls := "reveal",
        <.div(
          ^.cls := "slides",
          introduction,
          exercises,
          repl
        )
      )
    )
    .build

  @JSExport
  override def main(): Unit = {
    Show().renderIntoDOM(dom.document.body)
  }
}
