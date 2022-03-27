enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

// Basic settings -------------------------------

organization := "com.davegurnell"
name         := "bridges"

ThisBuild / scalaVersion       := "2.13.7"

ThisBuild / crossScalaVersions := Seq("2.13.7", "2.12.15")

ThisBuild / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Seq(
        "-feature",
        "-unchecked",
        "-deprecation",
        "-Xfatal-warnings",
        "-Ypartial-unification"
      )

    case Some((2, _)) =>
      Seq(
        "-feature",
        "-unchecked",
        "-deprecation",
        "-Xfatal-warnings",
      )

    case _ =>
      Seq(
        "-feature",
        "-unchecked",
        "-deprecation",
        "-rewrite",
        "-new-syntax",
      )
  }
}

ThisBuild / libraryDependencies ++= Seq(
  "com.chuusai"       %% "shapeless"          % "2.3.9",
  "com.davegurnell"   %% "unindent"           % "1.8.0",
  "org.apache.commons" % "commons-text"       % "1.9",
  "org.scalatest"     %% "scalatest"          % "3.2.10" % Test,
  "eu.timepit"        %% "refined"            % "0.9.27" % Provided,
  "eu.timepit"        %% "refined-shapeless"  % "0.9.27" % Provided
)

// Versioning -----------------------------------

ThisBuild / versionScheme := Some("early-semver")

git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty // Put "-SNAPSHOT" on a commit if it's not a tag

// Github Actions -------------------------------

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.11")

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")

ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

// Publishing -----------------------------------

usePgpKeyHex("2D2E2B8B8BBA48B5")

ThisBuild / licenses += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0"))

ThisBuild / homepage := Some(url("https://github.com/davegurnell/bridges"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/davegurnell/bridges.git"),
    "scm:git@github.com:davegurnell/bridges.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "davegurnell",
    name  = "Dave Gurnell",
    email = "dave@underscore.io",
    url   = url("https://twitter.com/davegurnell")
  )
)
