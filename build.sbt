name         in ThisBuild := "bridges"
organization in ThisBuild := "com.davegurnell"
version      in ThisBuild := "0.5.0"

scalaVersion       in ThisBuild := "2.12.6"
crossScalaVersions in ThisBuild := Seq("2.11.9", "2.12.6")

licenses += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0"))

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
  "org.scalatest"     %% "scalatest"     % "3.0.5" % Test
)

publishTo := sonatypePublishTo.value

publishMavenStyle := true

pomExtra in Global := {
  <url>https://github.com/davegurnell/bridges</url>
  <scm>
    <connection>scm:git:github.com/davegurnell/bridges</connection>
    <developerConnection>scm:git:git@github.com:davegurnell/bridges</developerConnection>
    <url>github.com/davegurnell/bridges</url>
  </scm>
  <developers>
    <developer>
      <id>davegurnell</id>
      <name>Dave Gurnell</name>
      <url>http://twitter.com/davegurnell</url>
    </developer>
  </developers>
}

scalafmtOnCompile := true
