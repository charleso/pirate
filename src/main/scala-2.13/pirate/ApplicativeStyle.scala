package pirate

import scalaz._
import Scalaz._

object ApplicativeStyle extends ApplicativeStyle

trait ApplicativeStyle {
  implicit class Function1ApplicativeStyle[A, B](fab: A => B) {
    def |*|[Z[_]](a: Z[A])(implicit Z: Applicative[Z]): Z[B] =
      a.map(fab)
  }

  implicit class Function2ApplicativeStyle[A, B, C](fab: (A, B) => C) {
    def |*|[Z[_]](x: (Z[A], Z[B]))(implicit Z: Applicative[Z]): Z[C] = {
      val (a, b) = x
      Z.apply2(a, b)(fab)
    }
  }

  implicit class Function3ApplicativeStyle[A, B, C, D](fab: (A, B, C) => D) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C]))(implicit Z: Applicative[Z]): Z[D] = {
      val (a, b, c) = x
      Z.apply3(a, b, c)(fab)
    }
  }

  implicit class Function4ApplicativeStyle[A, B, C, D, E](fab: (A, B, C, D) => E) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D]))(implicit Z: Applicative[Z]): Z[E] = {
      val (a, b, c, d) = x
      Z.apply4(a, b, c, d)(fab)
    }
  }

  implicit class Function5ApplicativeStyle[A, B, C, D, E, F](fab: (A, B, C, D, E) => F) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E]))(implicit Z: Applicative[Z]): Z[F] = {
      val (a, b, c, d, e) = x
      Z.apply5(a, b, c, d, e)(fab)
    }
  }

  implicit class Function6ApplicativeStyle[A, B, C, D, E, F, G](fab: (A, B, C, D, E, F) => G) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F]))(implicit Z: Applicative[Z]): Z[G] = {
      val (a, b, c, d, e, f) = x
      Z.apply6(a, b, c, d, e, f)(fab)
    }
  }

  implicit class Function7ApplicativeStyle[A, B, C, D, E, F, G, H](fab: (A, B, C, D, E, F, G) => H) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G]))(implicit Z: Applicative[Z]): Z[H] = {
      val (a, b, c, d, e, f, g) = x
      Z.apply7(a, b, c, d, e, f, g)(fab)
    }
  }

  implicit class Function8ApplicativeStyle[A, B, C, D, E, F, G, H, I](fab: (A, B, C, D, E, F, G, H) => I) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H]))(implicit Z: Applicative[Z]): Z[I] = {
      val (a, b, c, d, e, f, g, h) = x
      Z.apply8(a, b, c, d, e, f, g, h)(fab)
    }
  }

  implicit class Function9ApplicativeStyle[A, B, C, D, E, F, G, H, I, J](fab: (A, B, C, D, E, F, G, H, I) => J) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I]))(implicit Z: Applicative[Z]): Z[J] = {
      val (a, b, c, d, e, f, g, h, i) = x
      Z.apply9(a, b, c, d, e, f, g, h, i)(fab)
    }
  }

  implicit class Function10ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K](fab: (A, B, C, D, E, F, G, H, I, J) => K) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J]))(implicit Z: Applicative[Z]): Z[K] = {
      val (a, b, c, d, e, f, g, h, i, j) = x
      Z.apply10(a, b, c, d, e, f, g, h, i, j)(fab)
    }
  }

  implicit class Function11ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L](fab: (A, B, C, D, E, F, G, H, I, J, K) => L) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K]))(implicit Z: Applicative[Z]): Z[L] = {
      val (a, b, c, d, e, f, g, h, i, j, k) = x
      Z.apply11(a, b, c, d, e, f, g, h, i, j, k)(fab)
    }
  }

  implicit class Function12ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M](fab: (A, B, C, D, E, F, G, H, I, J, K, L) => M) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L]))(implicit Z: Applicative[Z]): Z[M] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l) = x
      Z.apply12(a, b, c, d, e, f, g, h, i, j, k, l)(fab)
    }
  }

  implicit class Function13ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M]))(implicit Z: Applicative[Z]): Z[N] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m) = x
      Z.apply3(Z.apply6(a, b, c, d, e, f)((_, _, _, _, _, _)), Z.apply6(g, h, i, j, k, l)((_, _, _, _, _, _)), m)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x3))
    }
  }

  implicit class Function14ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N]))(implicit Z: Applicative[Z]): Z[O] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = x
      Z.apply2(Z.apply7(a, b, c, d, e, f, g)((_, _, _, _, _, _, _)), Z.apply7(h, i, j, k, l, m, n)((_, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7))
    }
  }

  implicit class Function15ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O]))(implicit Z: Applicative[Z]): Z[P] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = x
      Z.apply3(Z.apply7(a, b, c, d, e, f, g)((_, _, _, _, _, _, _)), Z.apply7(h, i, j, k, l, m, n)((_, _, _, _, _, _, _)), o)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x3))
    }
  }

  implicit class Function16ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P]))(implicit Z: Applicative[Z]): Z[Q] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = x
      Z.apply2(Z.apply8(a, b, c, d, e, f, g, h)((_, _, _, _, _, _, _, _)), Z.apply8(i, j, k, l, m, n, o, p)((_, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8))
    }
  }

  implicit class Function17ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q]))(implicit Z: Applicative[Z]): Z[R] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = x
      Z.apply3(Z.apply8(a, b, c, d, e, f, g, h)((_, _, _, _, _, _, _, _)), Z.apply8(i, j, k, l, m, n, o, p)((_, _, _, _, _, _, _, _)), q)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x3))
    }
  }

  implicit class Function18ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q], Z[R]))(implicit Z: Applicative[Z]): Z[S] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = x
      Z.apply2(Z.apply9(a, b, c, d, e, f, g, h, i)((_, _, _, _, _, _, _, _, _)), Z.apply9(j, k, l, m, n, o, p, q, r)((_, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9))
    }
  }

  implicit class Function19ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q], Z[R], Z[S]))(implicit Z: Applicative[Z]): Z[T] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = x
      Z.apply3(Z.apply9(a, b, c, d, e, f, g, h, i)((_, _, _, _, _, _, _, _, _)), Z.apply9(j, k, l, m, n, o, p, q, r)((_, _, _, _, _, _, _, _, _)), s)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x3))
    }
  }

  implicit class Function20ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q], Z[R], Z[S], Z[T]))(implicit Z: Applicative[Z]): Z[U] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = x
      Z.apply2(Z.apply10(a, b, c, d, e, f, g, h, i, j)((_, _, _, _, _, _, _, _, _, _)), Z.apply10(k, l, m, n, o, p, q, r, s, t)((_, _, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10))
    }
  }

  implicit class Function21ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q], Z[R], Z[S], Z[T], Z[U]))(implicit Z: Applicative[Z]): Z[V] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = x
      Z.apply3(Z.apply10(a, b, c, d, e, f, g, h, i, j)((_, _, _, _, _, _, _, _, _, _)), Z.apply10(k, l, m, n, o, p, q, r, s, t)((_, _, _, _, _, _, _, _, _, _)), u)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10, x3))
    }
  }

  implicit class Function22ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W) {
    def |*|[Z[_]](x: (Z[A], Z[B], Z[C], Z[D], Z[E], Z[F], Z[G], Z[H], Z[I], Z[J], Z[K], Z[L], Z[M], Z[N], Z[O], Z[P], Z[Q], Z[R], Z[S], Z[T], Z[U],  Z[V]))(implicit Z: Applicative[Z]): Z[W] = {
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = x
      Z.apply2(Z.apply11(a, b, c, d, e, f, g, h, i, j, k)((_, _, _, _, _, _, _, _, _, _, _)), Z.apply11(l, m, n, o, p, q, r, s, t, u, v)((_, _, _, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x1._11, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10, x2._11))
    }
  }

}
