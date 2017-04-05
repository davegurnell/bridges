name         in ThisBuild := "shorthand"
organization in ThisBuild := "com.davegurnell"
version      in ThisBuild := "0.1"

scalaOrganization := "org.typelevel"
scalaVersion      := "2.12.1"

libraryDependencies ++= Seq(
  "com.chuusai"     %% "shapeless" % "2.3.2",
  "com.davegurnell" %% "unindent"  % "1.1.0",
  "org.scalatest"   %% "scalatest" % "3.0.1"
)
