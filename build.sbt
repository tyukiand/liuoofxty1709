version := "0.1.0"
scalaVersion := "2.12.7"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.8.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
organization := "io.github.tyukiand"

lazy val generateReadme = taskKey[Unit]("generates readme with coverage badges")
generateReadme := {
  val readmeSrc = IO.read(file("readmesrc/README.md"))

  /** URL for parameterized Shields.io badge */
  def shieldUrl(label: String, message: String, color: String): String = {
   s"""https://img.shields.io/static/v1.svg?label=${label}&message=${message}&color=${color}"""
  }
  
  /** URL embedded in Markdown */
  def shieldMd(label: String, message: String, color: String): String = {
    s""" ![${label}: ${message}](${shieldUrl(label, message, color)})"""
  }

  /** Badge for numeric values with red-yellow-green colors */
  def shieldMdThresh(
    label: String,
    value: Double,
    yellowThresh: Double,
    greenThresh: Double,
    units: String = ""
  ): String = {
    val color = 
      if (value >= greenThresh) "green" else
      if (value >= yellowThresh) "yellow" else
      if (value > 0) "orange" else
      "red"
    shieldMd(label, value.toString + units, color)
  }

  val buildSuccessful = (Compile / compile).result.value match {
    case Value(_) => true
    case _ => false
  }

  val buildBadge = if (buildSuccessful) {
    shieldMd("local-build", "passing", "green")
  } else {
    shieldMd("local-build", "failing", "red")
  }

  val testsSuccessful = (Test / test).result.value match {
    case Value(_) => true
    case _ => false
  }

  val testBadge = if (testsSuccessful) {
    shieldMd("local-tests", "passing", "green")
  } else {
    shieldMd("local-tests", "failing", "red")
  }

  val sbtVers = sbtVersion.value
  val scalVers = scalaVersion.value
  import java.time.ZonedDateTime
  import java.time.format.DateTimeFormatter._

  val now = java.time.ZonedDateTime.now.format(ISO_LOCAL_DATE)

  import scala.sys.process._
  val systemDescription = "uname -o -p".!!
  
  val badgeDescription = 
    s""" *Tested locally on ${systemDescription} with """ + 
    s"""`scalaVersion = ${scalVers}`, `sbtVersion = ${sbtVers}`.""" + 
    s""" Readme generated on ${now}.* """

  import scala.xml.XML
  val f = file("target/scala-2.12/scoverage-report/scoverage.xml")
  val coverageBadges =
    (if (f.exists) {
      val report = XML.loadFile(f)
      val branchCov = report.attribute("branch-rate").get.text.toDouble
      val stmtCov = report.attribute("statement-rate").get.text.toDouble

      shieldMdThresh("statement-coverage", stmtCov, 50, 90, "%") + " " +
      shieldMdThresh("branch-coverage", branchCov, 50, 90, "%")
    } else {
      shieldMd("coverage", "unavailable", "red")
    })

  val badges = 
    shieldMd("version", version.value, "lightgrey") + 
    buildBadge + 
    testBadge +
    coverageBadges + 
    s"\n\n ${badgeDescription}"

  val readmeWithBadges = readmeSrc.replaceAll("<BADGES>", badges)

  val FileSnippetRegex = "( *)<FILE:(.*)>".r

  val readmeWithSnippets = FileSnippetRegex.replaceAllIn(
    readmeWithBadges,
    m => {
      val indentation = m.group(1)
      val lines = scala.io.Source.fromFile(m.group(2)).getLines.toList
      val codeLines = if (lines.exists(_.contains("CROP-HEADER"))) {
        lines.dropWhile(l => !l.contains("CROP-HEADER")).tail
      } else lines
      codeLines.map(indentation.+).mkString("\n")
    }
  )

  IO.write(file("./README.md"), readmeWithSnippets)
}

