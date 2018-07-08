name         in ThisBuild := "bridges"
organization in ThisBuild := "com.davegurnell"

scalaVersion       in ThisBuild := "2.12.6"
crossScalaVersions in ThisBuild := Seq("2.12.6")

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xfatal-warnings",
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "com.chuusai"       %% "shapeless"     % "2.3.3",
  "com.davegurnell"   %% "unindent"      % "1.1.0",
  "org.apache.commons" % "commons-lang3" % "3.5",
  "org.scalatest"     %% "scalatest"     % "3.0.5" % Test,
  "eu.timepit"        %% "refined"       % "0.9.0" % Test
)

// Versioning

// A lot of the versioning, publishing, and Travis-related code below is adapted from:
//
//   - https://alexn.org/blog/2017/08/16/automatic-releases-sbt-travis.html
//   - http://caryrobbins.com/dev/sbt-publishing/

enablePlugins(GitVersioning)

git.baseVersion := "0.8.0"

val ReleaseTag = """^([\d\.]+)$""".r

git.gitTagToVersionNumber := {
  case ReleaseTag(v) => Some(v)
  case _             => None
}

git.formattedShaVersion := {
  // val suffix = git.makeUncommittedSignifierSuffix(
  //   git.gitUncommittedChanges.value,
  //   git.uncommittedSignifier.value)

  git.gitHeadCommit.value.map(_.substring(0, 7)).map { sha =>
    // s"${git.baseVersion.value}-${sha}-SNAPSHOT"

    // Actually let's not put the SHA in there... we'll end up with hundreds of excess snapshots:
    s"${git.baseVersion.value}-SNAPSHOT"
  }
}

// Publishing

publishMavenStyle := true

isSnapshot := version.value endsWith "SNAPSHOT"

publishTo := sonatypePublishTo.value

usePgpKeyHex("2D2E2B8B8BBA48B5")

pgpPublicRing := baseDirectory.value / "project" / ".gnupg" / "pubring.gpg"
pgpSecretRing := baseDirectory.value / "project" / ".gnupg" / "secring.gpg"

licenses += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/davegurnell/bridges"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/davegurnell/bridges.git"),
    "scm:git@github.com:davegurnell/bridges.git"))

developers := List(
  Developer(
    id    = "davegurnell",
    name  = "Dave Gurnell",
    email = "dave@underscore.io",
    url   = url("https://twitter.com/davegurnell")))

// Travis

// Sonatype credentials are on Travis in a secret:
credentials ++= {
  val travisCredentials = for {
    user <- sys.env.get("SONATYPE_USER")
    pass <- sys.env.get("SONATYPE_PASS")
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)

  travisCredentials.toSeq
}

// Password to the PGP certificate is on Travis in a secret:
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)

addCommandAlias("ci", ";clean ;coverage ;compile ;test ;coverageReport ;package")
addCommandAlias("release", ";+publishSigned ;sonatypeReleaseAll")
