# Bridges

Generate bindings for Scala types in other programming languages.

Copyright 2017 Dave Gurnell. Licensed [Apache 2.0][license].

[![Build Status](https://travis-ci.org/davegurnell/bridges.svg?branch=develop)](https://travis-ci.org/davegurnell/bridges)
[![Coverage status](https://img.shields.io/codecov/c/github/davegurnell/bridges/develop.svg)](https://codecov.io/github/davegurnell/bridges)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.underscore/bridges_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.underscore/bridges_2.11)

## Getting Started

Grab the code by adding the following to your `build.sbt`:

~~~ scala
libraryDependencies += "com.davegurnell" %% "bridges" % "<<VERSION>>"
~~~

## Synopsis

### Render Typescript Bindings for Scala ADTs

Create a simple data model:

~~~ scala
final case class Color(red: Int, green: Int, blue: Int)

sealed abstract class Shape extends Product with Serializable
final case class Circle(radius: Double, color: Color) extends Shape
final case class Rectangle(width: Double, height: Double, color: Color) extends Shape
~~~

Call `bindings[Foo].render` to render a type and all its dependendents
to a `String` representing a Typescript definition:

~~~ scala
import bridges.ts._

bindings[Shape].render
// res1: String =
// type Color = {
//   red: number,
//   green: number,
//   blue: number
// }
//
// type Circle = {
//   radius: number,
//   color: Color
// }
//
// type Rectangle = {
//   width: number,
//   height: number,
//   color: Color
// }
//
// type Shape =
//   Circle |
//   Rectangle
~~~

Call `bindings[Foo].writeTo(file)` to write the definitions to a file:

~~~ scala
import java.io.File

bindings[Shape].writeTo(new File("/tmp/shape.ts"))
// Unit
~~~

[license]: http://www.apache.org/licenses/LICENSE-2.0
