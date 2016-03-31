package com.gvolpe.free.reasonablemonads

import algebra._
import freelibrary.{~>, Monad}

object interpreters {

  /**
  * Console Interpreter definition
  * */
  type Id[A] = A

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A) = a
    def flatMap[A,B](a: A)(f: A => B) = f(a)
  }

  object ConsoleInterpreter extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine
      case Tell(msg) =>
        println(msg)
    }
  }

  /**
  * Tester Interpreter definition
  * */
  type Tester[A] = Map[String, String] => (List[String], A)

  implicit val testerMonad = new Monad[Tester] {
    def pure[A](a: A) = _ => (List(), a)
    def flatMap[A,B](t: Tester[A])(f: A => Tester[B]) =
      m => {
        val (o1, a) = t(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }
  }

  object TestConsoleInterpreter extends (Interact ~> Tester) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) => m => (List(), m(prompt))
      case Tell(msg) => _ => (List(msg), ())
    }
  }

  /**
  * TestAuth Interpreter definition
  * */
  object TestAuth extends (Auth ~> Id) {
    def apply[A](a: Auth[A]) = a match {
      case Login(uid, pwd) =>
        if (uid == "john.snow" && pwd == "Ghost")
          Some(User("john.snow"))
        else None
      case HasPermission(u, _) =>
        u.id == "john.snow"
    }
  }

}
