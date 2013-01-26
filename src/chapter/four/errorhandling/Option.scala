package chapter.four.errorhandling

/**
 * Created with IntelliJ IDEA.
 * User: nasoloaina
 * Date: 1/25/13
 * Time: 5:12 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(_) => Some(f(_))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(_) => _
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse None

  def filter(f: A => Boolean): Option[A] = this map (Some(_)) getOrElse None

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
