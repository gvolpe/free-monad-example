package com.gvolpe.free.reasonablemonads

import algebra._
import freelibrary._
import scala.language.higherKinds

object liftings {

  class Interacts[F[_]](implicit I: Inject[Interact,F]) {
    def tell(msg: String): Free[F,Unit] = lift(Tell(msg))
    def ask(prompt: String): Free[F,String] = lift(Ask(prompt))
  }

  class Auths[F[_]](implicit I: Inject[Auth,F]) {
    def login(id: UserID, pwd: Password): Free[F,Option[User]] = lift(Login(id, pwd))
    def hasPermission(u: User, p: Permission): Free[F,Boolean] = lift(HasPermission(u, p))
  }

  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth,F]): Auths[F] = new Auths[F]
  }

  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact,F]): Interacts[F] = new Interacts[F]
  }

}
