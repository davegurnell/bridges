package bridges

sealed abstract class Target extends Product with Serializable

sealed abstract class Typescript extends Target
case object Typescript extends Typescript

sealed abstract class Flow extends Target
case object Flow extends Flow

sealed abstract class Elm extends Target
case object Elm extends Elm
