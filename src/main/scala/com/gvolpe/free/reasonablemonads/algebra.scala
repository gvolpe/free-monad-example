package com.gvolpe.free.reasonablemonads

object algebra {

  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  type UserID = String
  type Password = String
  type Permission = String
  case class User(id: String)

  sealed trait Auth[A]
  case class Login(u: UserID, p: Password) extends Auth[Option[User]]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

}
