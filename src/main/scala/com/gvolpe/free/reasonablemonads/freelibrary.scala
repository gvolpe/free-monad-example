package com.gvolpe.free.reasonablemonads

import scala.language.higherKinds

object freelibrary {

  /**
  * Monad definition
  * */
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A,B](a: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
  }

  /**
  * Natural transformation definition
  * */
  trait ~>[F[_],G[_]] { self =>
    def apply[A](f: F[A]): G[A]

    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case Left(fa) => self(fa)
          case Right(ha) => f(ha)
        }
      }
  }

  /**
  * Free Monad definition
  * */
  trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(fx, g) =>
          Bind(fx, g andThen (_ flatMap f))
      }

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap[G[_]:Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case Bind(fx, g) =>
          Monad[G].flatMap(f(fx)) { a =>
            g(a).foldMap(f)
          }
      }
  }

  /**
  * Free Monad algebra
  * */
  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Bind[F[_],I,A](a: F[I], f: I => Free[F,A]) extends Free[F,A]

  /**
  * Coproduct definition for composability
  * */
  case class Coproduct[F[_],G[_],A](run: Either[F[A],G[A]])

  /**
  * Inject definition in order to work with Coproducts
  * */
  trait Inject[F[_],G[_]] {
    def inj[A](sub: F[A]): G[A]
    def prj[A](sup: G[A]): Option[F[A]]
  }

  object Inject {
    implicit def injRefl[F[_]] = new Inject[F,F] {
      def inj[A](sub: F[A]) = sub
      def prj[A](sup: F[A]) = Some(sup)
    }

    implicit def injLeft[F[_],G[_]] = new Inject[F,({type λ[α] = Coproduct[F,G,α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct(Left(sub))
      def prj[A](sup: Coproduct[F,G,A]) = sup.run match {
        case Left(fa) => Some(fa)
        case Right(_) => None
      }
    }

    implicit def injRight[F[_],G[_],H[_]](implicit I: Inject[F,G]) =
      new Inject[F,({type f[x] = Coproduct[H,G,x]})#f] {
        def inj[A](sub: F[A]) = Coproduct(Right(I.inj(sub)))
        def prj[A](sup: Coproduct[H,G,A]) = sup.run match {
          case Left(_) => None
          case Right(x) => I.prj(x)
        }
      }
  }

  def lift[F[_],G[_],A](f: F[A])(implicit I: Inject[F,G]): Free[G,A] = Bind(I.inj(f), Return(_:A))

}
