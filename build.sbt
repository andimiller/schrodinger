import sbtwelcome._

// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization         := "net.andimiller"
ThisBuild / organizationName     := "andimiller"
ThisBuild / organizationHomepage := Some(url("https://andimiller.net"))
ThisBuild / startYear            := Some(2023)
ThisBuild / licenses             := Seq(License.Apache2)
ThisBuild / developers           := List(
  // your GitHub handle and name
  tlGitHubDev("andimiller", "Andi Miller")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := None // don't publish yet

val Scala213 = "2.13.12"
ThisBuild / crossScalaVersions                  := Seq(Scala213, "3.3.1")
ThisBuild / scalaVersion                        := Scala213 // the default Scala
ThisBuild / githubWorkflowPublishTargetBranches := Seq()    // CI doesn't need to publish, I'll do that myself for now
ThisBuild / githubWorkflowJavaVersions          := List(JavaSpec.temurin("11"))

lazy val root = tlCrossRootProject
  .aggregate(core, simple, hash4j)
  .settings(
    logoColor   := scala.Console.MAGENTA,
    logo        := s"""
███████╗ ██████╗██╗  ██╗██████╗  ██████╗ ██████╗ ██╗███╗   ██╗ ██████╗ ███████╗██████╗ 
██╔════╝██╔════╝██║  ██║██╔══██╗██╔═══██╗██╔══██╗██║████╗  ██║██╔════╝ ██╔════╝██╔══██╗
███████╗██║     ███████║██████╔╝██║   ██║██║  ██║██║██╔██╗ ██║██║  ███╗█████╗  ██████╔╝
╚════██║██║     ██╔══██║██╔══██╗██║   ██║██║  ██║██║██║╚██╗██║██║   ██║██╔══╝  ██╔══██╗
███████║╚██████╗██║  ██║██║  ██║╚██████╔╝██████╔╝██║██║ ╚████║╚██████╔╝███████╗██║  ██║
╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═════╝ ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝

${version.value}

${scala.Console.YELLOW}Scala ${scalaVersion.value}${scala.Console.RESET}
""",
    usefulTasks := Seq(
      UsefulTask("test", "Test with current config").alias("t"),
      UsefulTask(
        "scalafmtAll; scalafmtSbt; scalafixAll; headerCreateAll",
        "Fix all formatting"
      ).alias("f")
    )
  )

lazy val commonSettings = Seq(
  logo := ""
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"        % "2.9.0",
      "org.scodec"    %%% "scodec-bits"      % "1.1.38",
      "org.typelevel" %%% "spire"            % "0.18.0",
      "org.scalameta" %%% "munit"            % "1.0.0-M10" % Test,
      "org.typelevel" %%% "cats-laws"        % "2.9.0"     % Test,
      "org.typelevel" %%% "discipline-munit" % "2.0.0-M3"  % Test,
      "org.typelevel" %%% "spire-laws"       % "0.18.0"    % Test
    )
  )

lazy val simple = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("simple"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings: _*)
  .settings(
    name := "schrodinger-simple",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"        % "2.9.0",
      "org.scodec"    %%% "scodec-bits"      % "1.1.38",
      "org.scodec"    %%% "scodec-cats"      % "1.2.0",
      "org.scalameta" %%% "munit"            % "1.0.0-M10" % Test,
      "org.typelevel" %%% "cats-laws"        % "2.9.0"     % Test,
      "org.typelevel" %%% "discipline-munit" % "2.0.0-M3"  % Test
    ),
    libraryDependencies +=
      (scalaBinaryVersion.value match {
        case "2.13"                 =>
          ("org.scodec" %%% "scodec-core" % "1.11.10")
        case s if s.startsWith("3") =>
          ("org.scodec" %%% "scodec-core" % "2.2.2")
      })
  )

lazy val hash4j = crossProject(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("hash4j"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings: _*)
  .settings(
    name := "schrodinger-hash4j",
    libraryDependencies ++= Seq(
      "com.dynatrace.hash4j" % "hash4j"           % "0.13.0",
      "org.scalameta"      %%% "munit"            % "1.0.0-M10" % Test,
      "org.typelevel"      %%% "cats-laws"        % "2.9.0"     % Test,
      "org.typelevel"      %%% "discipline-munit" % "2.0.0-M3"  % Test
    )
  )

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    logo := "" // TODO add useful tasks here for docs stuff
  )
