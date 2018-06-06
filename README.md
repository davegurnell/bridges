# Bridges

Generate bindings for Scala types in other programming languages.

Copyright 2017 Dave Gurnell. Licensed [Apache 2.0][license].

[![Build Status](https://travis-ci.org/davegurnell/bridges.svg?branch=develop)](https://travis-ci.org/davegurnell/bridges)
[![Coverage status](https://img.shields.io/codecov/c/github/davegurnell/bridges/develop.svg)](https://codecov.io/github/davegurnell/bridges)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.davegurnell/bridges_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.davegurnell/bridges_2.12)

## Getting Started

Grab the code by adding the following to your `build.sbt`:

~~~ scala
libraryDependencies += "com.davegurnell" %% "bridges" % "<<VERSION>>"
~~~

## Synopsis

### Render Typescript/Flow Declarations for Scala ADTs

Create a simple data model:

~~~ scala
final case class Color(red: Int, green: Int, blue: Int)

sealed abstract class Shape extends Product with Serializable
final case class Circle(radius: Double, color: Color) extends Shape
final case class Rectangle(width: Double, height: Double, color: Color) extends Shape
~~~

Call `declaration[Foo]` to generate a type-declaration for `Foo`.
Call `render[Typescript](...)` to convert a list of declarations as Typescript,
or `render[Flow](...)` to render them as Flow types.

~~~ scala
import bridges._
import bridges.syntax._

render(List(
  declaration[Color],
  declaration[Circle],
  declaration[Rectangle],
  declaration[Shape]
))
// res1: String =
// export type Color = {
//   red: number,
//   green: number,
//   blue: number
// };
//
// export type Circle = {
//   radius: number,
//   color: Color
// };
//
// export type Rectangle = {
//   width: number,
//   height: number,
//   color: Color
// };
//
// export type Shape =
//   ({ type: "Circle" } & Circle) |
//   ({ type: "Rectangle" } & Rectangle);
~~~

[license]: http://www.apache.org/licenses/LICENSE-2.0
