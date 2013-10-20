package chapter.four.errorhandling

/**
 * Created with IntelliJ IDEA.
 * User: nasoloaina
 * Date: 1/25/13
 * Time: 5:12 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait Option[+A] {

  /**
   * EXERCISE 1
   * Implementing trait Option function
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(b) => Some(f(b))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(b) => b
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse None

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
