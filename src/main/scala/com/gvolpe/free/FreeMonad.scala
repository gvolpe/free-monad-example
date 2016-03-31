package com.gvolpe.free

import scala.language.higherKinds
import scalaz._

object FreeMonad extends App {

  // Defining the algebra for operations
  sealed trait Operation[A]
  case object Get extends Operation[String]
  case class Set(value: String) extends Operation[Unit]

  // Free monad over the free functor of Operation
  type FreeOperation[A] = Free.FreeC[Operation, A]

  implicit val monadOperation: Monad[FreeOperation] = Free.freeMonad[({type λ[α] = Coyoneda[Operation, α]})#λ]

  // Operations lifted to the Free Monad Coyoneda
  def get: FreeOperation[String] = Free.liftFC(Get)
  def set(value: String): FreeOperation[Unit] = Free.liftFC(Set(value))

  // Definition of the program
  val program1 = for {
    x <- get
    _ <- set(x)
  } yield ()

  val program2 = for {
    x <- get
    y <- get
    _ <- set(y)
    _ <- set(x)
  } yield ()

  // Mock for an IO Operation
  case class Mock(in: List[String], out: List[String])
  object Mock {
    def read(mock: Mock): (Mock, String) = mock.in match {
      case Nil    => (mock, "")
      case h :: t => (mock.copy(in = t), h)
    }
    def write(value: String)(mock: Mock): Mock = mock.copy(out = value :: mock.out)
  }

  // Natural transformation to a Mock State
  type MockState[A] = State[Mock, A]

  def operationToState: Operation ~> MockState = new (Operation ~> MockState) {
    def apply[A](op: Operation[A]): MockState[A] = op match {
      case Get => State(Mock.read)
      case Set(value) => State.modify(Mock.write(value))
    }
  }

  val init = Mock(in = List("Hello", "World"), out = List())

  // Running the Definition (program2) with the Interpreter (Operation to State)
  val result = Free.runFC(program2)(operationToState).exec(init).out
  println(result)

}