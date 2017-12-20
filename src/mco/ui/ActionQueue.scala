package mco.ui

import monix.eval.Coeval
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject


class ActionQueue {
  private val subject = PublishSubject[Coeval[Unit]]()

  def enqueue(coeval: Coeval[Unit]): Unit = {
    subject.onNext(coeval)
    ()
  }

  val watch: Observable[Throwable] =
    subject
      .mapTask(_.task.executeWithFork.attempt)
      .collect { case Left(exception) => exception }
}
