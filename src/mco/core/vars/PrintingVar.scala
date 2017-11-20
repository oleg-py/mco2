package mco.core.vars

/**
  * Var decorator useful for debugging / manual testing
  * Technically it's side-effecting, but logging is not
  * observable to the rest of the program
  *
  * @param underlying - actual variable to use for storing
  * @tparam F effect type
  * @tparam A value type
  */
class PrintingVar[F[_], A](
  underlying: Var[F, A],
  tag: String = "var-change"
) extends Var[F, A] {
  override def apply(): F[A] = underlying()
  override def :=(a: A): F[Unit] = {
    pprint.log(a, tag)
    underlying := a
  }
}
