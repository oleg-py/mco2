package mco.core.vars

import cats._
import cats.syntax.all._


/**
 * A Var decorator that combines two variables, such that
 * read operations are only directed to one of them, but
 * write operations are directed to both
 *
 * @param base - variable used for writing only
 * @param reads - variable used for both reading and writing
 * @tparam F effect type of alterations (must have Apply instance)
 * @tparam A value type
 */
class CacheVar[F[_]: Apply, A](base: Var[F, A], reads: Var[F, A])
  extends Var[F, A]
{
  override def apply(): F[A] = reads()
  override def :=(a: A): F[Unit] = (base := a) *> (reads := a)
  override def ~=(f: A => A)(implicit F: FlatMap[F]): F[Unit] =
    (reads ~= f) *> reads() >>= { base := _ }
}

object CacheVar {
  /**
   * Create a cached Var, given existing var and factories
   * for default value and a Var used for cached access
   *
   * @param read - operation used to generate default value
   *             if the base fails to be read
   * @param base - variable used for initial read which will
   *             be used only for writing later
   * @param mkReads - function creating a "cache" variable
   *                that will be used for both reading and writing
   * @tparam F - monad with support for error recovery
   * @tparam A - stored value
   * @tparam E - error type of monad
   * @return
   */
  def apply[F[_], A, E](
    read: => F[A]
  )(
    base: Var[F, A],
    mkReads: A => F[Var[F, A]]
  )(implicit
    F: MonadError[F, E]
  ): F[Var[F, A]] =
    base().handleErrorWith(_ => read)
      .flatMap(mkReads)
      .map(new CacheVar(base, _))
}