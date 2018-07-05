name         in ThisBuild := "bridges"
organization in ThisBuild := "com.davegurnell"

scalaVersion       in ThisBuild := "2.12.6"
crossScalaVersions in ThisBuild := Seq(
  // "2.11.12",
  "2.12.6",
)

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

enablePlugins(GitVersioning)

git.baseVersion := "0.7.0"

val ReleaseTag = """^([\d\.]+)$""".r
git.gitTagToVersionNumber := {
  case ReleaseTag(v) => Some(v)
  case _             => None
}

git.formattedShaVersion := {
  val suffix = git.makeUncommittedSignifierSuffix(
    git.gitUncommittedChanges.value,
    git.uncommittedSignifier.value)

  git.gitHeadCommit.value.map(_.substring(0, 7)).map { sha =>
    s"${git.baseVersion.value}-${sha}${suffix}"
  }
}

// Publishing

// Code taken from https://alexn.org/blog/2017/08/16/automatic-releases-sbt-travis.html

credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  sys.env.getOrElse("SONATYPE_USER", ""),
  sys.env.getOrElse("SONATYPE_PASS", ""))

publishMavenStyle := true

isSnapshot := version.value endsWith "SNAPSHOT"

publishTo := sonatypePublishTo.value

useGpg := false
usePgpKeyHex("2D2E2B8B8BBA48B5")
pgpPublicRing := baseDirectory.value / "project" / ".gnupg" / "pubring.gpg"
pgpSecretRing := baseDirectory.value / "project" / ".gnupg" / "secring.gpg"
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)

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

addCommandAlias("ci", ";clean ;coverage ;compile ;test ;coverageReport ;package")
addCommandAlias("release", ";+publishSigned ;sonatypeReleaseAll")
