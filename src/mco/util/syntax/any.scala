package mco.util.syntax


object any {
  implicit class AnyTypeSyntax[A](val a: A) extends AnyVal {
    @inline def pipe[B](f: A => B): B = f(a)
    @inline def tap[B](f: A => B): A = { f(a); a }
  }

  val unit: Unit = ()
}
