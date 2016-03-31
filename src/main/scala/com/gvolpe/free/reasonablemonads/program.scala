package com.gvolpe.free.reasonablemonads

import algebra._
import freelibrary._
import interpreters._
import liftings._
import scala.language.higherKinds

object program extends App {

  val KnowSecret = "KnowSecret"

  /**
  * Requiring the evidence that our program can combine Interacts and Auths free monads
  * */
  def program[F[_]](implicit I: Interacts[F], A: Auths[F]) = {
    import I._; import A._
    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      u <- login(uid, pwd)
      b <- u.map(hasPermission(_, KnowSecret)).getOrElse(Return(false))
      _ <- if (b) tell("Th3S3cR3t;)") else tell("Go away!")
    } yield ()
  }

  /**
  * Alias for our App
  * */
  type App[A] = Coproduct[Auth, Interact, A]
  val app: Free[App, Unit] = program[App]

  /**
  * Running our app by passing the chosen interpreters
  * */
  def runApp = app.foldMap(TestAuth or ConsoleInterpreter)

}
