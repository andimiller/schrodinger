// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "net.andimiller"
ThisBuild / organizationName := "andimiller"
ThisBuild / organizationHomepage := Some(url("https://andimiller.net"))
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("andimiller", "Andi Miller")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := None // don't publish yet

val Scala213 = "2.13.12"
ThisBuild / crossScalaVersions := Seq(Scala213, "3.3.1")
ThisBuild / scalaVersion := Scala213 // the default Scala
ThisBuild / githubWorkflowPublishTargetBranches := Seq() // CI doesn't need to publish, I'll do that myself for now

lazy val root = tlCrossRootProject.aggregate(core, simple)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.typelevel" %%% "cats-effect" % "3.5.1",
      "org.scalameta" %%% "munit" % "1.0.0-M10" % Test,
      "org.typelevel" %%% "munit-cats-effect" % "2.0.0-M3" % Test
    )
  )

lazy val simple = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("simple"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-simple",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.scalameta" %%% "munit" % "1.0.0-M10" % Test,
      "org.typelevel" %%% "cats-laws" % "2.9.0" % Test,
      "org.typelevel" %%% "discipline-munit" % "2.0.0-M3" % Test
    )
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
