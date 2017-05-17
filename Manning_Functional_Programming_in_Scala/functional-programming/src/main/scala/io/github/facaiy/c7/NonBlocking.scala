package io.github.facaiy.c7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

/**
 * Created by facai on 5/10/17.
 */
object NonBlocking {
  sealed trait Future[A] {
    private[c7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  // ex 7.10
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)

    try {
      p(es){ a => ref.set(a); latch.countDown() }
      latch.await()
    } catch {
      case e: Exception =>
        latch.countDown()
        throw e
    }

    ref.get()
  }
}
