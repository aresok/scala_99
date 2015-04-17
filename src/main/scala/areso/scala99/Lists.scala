package areso.scala99

import scala.annotation.tailrec
import scala.concurrent.Future

object Lists {

  /** (P01) Find the last element of a list. */
  @tailrec
  def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case head :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  /** (P02) Find the last but one element of a list. */
  @tailrec
  def penultimate[A](list: List[A]): A = list match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case _ :: Nil => throw new NoSuchElementException
    case Nil => throw new NoSuchElementException
  }

  /** (P03) Find the Kth element of a list. */
  def nth[A](n: Int, list: List[A]): A = {
    @tailrec
    def nthInternal(idx: Int, l: List[A]): A = l match {
      case Nil => throw new NoSuchElementException
      case head :: _ if idx == n => head
      case _ :: tail => nthInternal(idx + 1, tail)
    }
    nthInternal(0, list)
  }

  /** (P04) Find the number of elements of a list. */
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthInternal(result: Int, list: List[A]): Int = list match {
      case Nil => result
      case _ :: tail => lengthInternal(result + 1, tail)
    }
    lengthInternal(0, list)
  }

  /** (P05) Reverse a list. */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseInternal(result: List[A], list: List[A]): List[A] = list match {
      case Nil => result
      case head :: tail => reverseInternal(head :: result, tail)
    }
    reverseInternal(List.empty[A], list)
  }

  /** (P06) Find out whether a list is a palindrome. */
  @tailrec
  def isPalindrome[A](list: List[A]): Boolean = list match {
    case Nil => true
    case head :: Nil => true
    case head :: tail if head == tail.last => isPalindrome(tail dropRight 1)
    case head :: tail => false
  }

  /** (P07) Flatten a nested list structure. */
  def flatten(list: List[Any]): List[Any] = {
    def flattenInternal(result: List[Any], list: List[Any]): List[Any] = list match {
      case head :: tail =>
        head match {
          case x: List[_] => flattenInternal(flattenInternal(result, x), tail)
          case el => flattenInternal(result :+ el, tail)
        }
      case Nil => result
    }
    flattenInternal(List.empty[Any], list)
  }

  /** (P07) Flatten a nested list structure. */
  def flatten2(list: List[Any]): List[Any] = list flatMap {
    case sublist: List[_] => flatten2(sublist)
    case element => List(element)
  }

  /** (P08) Eliminate consecutive duplicates of list elements. */
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def compressInternal[A](current: Option[A], acc: List[A], list: List[A]): List[A] = list match {
      case Nil => acc
      case head :: tail if current.contains(head) => compressInternal(current, acc, tail)
      case head :: tail => compressInternal(Some(head), acc :+ head, tail)
    }
    compressInternal(Option.empty[A], List.empty[A], list)
  }

  /** (P09) Pack consecutive duplicates of list elements into sublists. */
  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def packInternal[A](current: Option[A], accAll: List[List[A]], accSmall: Option[List[A]], list: List[A]): List[List[A]] = list match {
      case Nil => accSmall.fold(accAll)(accAll :+ _)
      case head :: tail if current.contains(head) => packInternal(current, accAll, accSmall.map(_ :+ head), tail)
      case head :: tail => packInternal(Some(head), accSmall.fold(accAll)(accAll :+ _), Some(List(head)), tail)
    }
    list match {
      case Nil => List(List.empty[A])
      case _ => packInternal(Option.empty[A], List.empty[List[A]], Option.empty[List[A]], list)
    }
  }

  /** (P10) Run-length encoding of a list (use result of P09). */
  def encode[A](list: List[A]): List[(Int, A)] =
    for (x <- pack(list) if x.nonEmpty) yield (x.length, x.head)

}
