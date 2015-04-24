package areso.scala99

import scala.annotation.tailrec

object Lists {

  /** (P01) Find the last element of a list. */
  @tailrec
  def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case head :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  /** (P01) Find the last element of a list. */
  def lastBuildIn[A](list: List[A]): A = list.last

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
          case x: List[_] => flattenInternal(result ::: flattenInternal(List.empty[Any], x), tail)
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
    def compressInternal(current: Option[A], acc: List[A], list: List[A]): List[A] = list match {
      case Nil => acc
      case head :: tail if current.contains(head) => compressInternal(current, acc, tail)
      case head :: tail => compressInternal(Some(head), acc :+ head, tail)
    }
    compressInternal(Option.empty[A], List.empty[A], list)
  }

  /** (P09) Pack consecutive duplicates of list elements into sublists. */
  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def packInternal(current: Option[A], accAll: List[List[A]], accSmall: Option[List[A]], list: List[A]): List[List[A]] = list match {
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

  /** (P11) Modified run-length encoding. */
  def encodeModified[A](list: List[A]): List[Any] = {
    for (x <- pack(list) if x.nonEmpty) yield if (x.length > 1) (x.length, x.head) else x.head
  }

  /** (P12) Decode a run-length encoded list. */
  def decode[A](list: List[(Int, A)]): List[A] = {
    def decodePair(acc: List[A], pair: (Int, A)): List[A] =
      acc ::: (for (x <- 1 to pair._1) yield pair._2).toList
    @tailrec
    def decodeInternal(acc: List[A], list: List[(Int, A)]): List[A] = list match {
      case Nil => acc
      case head :: tail => decodeInternal(decodePair(acc, head), tail)
    }
    decodeInternal(List.empty[A], list)
  }

  /** (P13) Run-length encoding of a list (direct solution). */
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    @tailrec
    def encodeInternal(acc: List[(Int, A)], current: Option[(Int, A)], list: List[A]): List[(Int, A)] = list match {
      case Nil => current.fold(acc)(acc :+ _)
      case head :: tail if current.exists(_._2 == head) => encodeInternal(acc, current.map(x => (x._1 + 1, head)), tail)
      case head :: tail => encodeInternal(current.fold(acc)(acc :+ _), None, tail)
    }
    encodeInternal(List.empty[(Int, A)], None, List.empty[A])
  }

  /** (P14) Duplicate the elements of a list. */
  def duplicate[A](list: List[A]): List[A] = {
    @tailrec
    def duplicateInternal(acc: List[A], list: List[A]): List[A] = list match {
      case Nil => acc
      case head :: tail => duplicateInternal(acc :+ head :+ head, tail)
    }
    duplicateInternal(List.empty[A], list)
  }

  def duplicate2[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

  /** (P15) Duplicate the elements of a list a given number of times. */
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def duplicateNInternal(acc: List[A], list: List[A]): List[A] = list match {
      case Nil => acc
      case head :: tail => duplicateNInternal(acc ::: (for (i <- 1 to n) yield head).toList, tail)
    }
    duplicateNInternal(List.empty[A], list)
  }

  /** (P16) Drop every Nth element from a list. */
  def drop[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def dropInternal(acc: List[A], counter: Int, list: List[A]): List[A] = (counter, list) match {
      case (_, Nil) => acc
      case (1, _ :: tail) => dropInternal(acc, n, tail)
      case (_, head :: tail) => dropInternal(acc :+ head, counter - 1, tail)
    }
    dropInternal(List.empty[A], n, list)
  }

  def dropFunctional[A](n: Int, ls: List[A]): List[A] = n match {
    case 0 => ls
    case _ => ls.zipWithIndex filter { el => (el._2 + 1) % n != 0 } map { _._1 }
  }

  /** (P17) Split a list into two parts. */
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitInternal(acc: (List[A], List[A]), counter: Int, list: List[A]): (List[A], List[A]) = list match {
      case Nil => acc
      case head :: tail if counter < n => splitInternal((acc._1 :+ head, acc._2), counter + 1, tail)
      case head :: tail => splitInternal((acc._1, acc._2 :+ head), counter + 1, tail)
    }
    splitInternal((Nil, Nil), 0, list)
  }

  def splitFunctional[A](n: Int, list: List[A]): (List[A], List[A]) = (list take n, list drop n)

  /** (P18) Extract a slice from a list */
  def slice[A](begin: Int, end: Int, list: List[A]): List[A] = (begin, end) match {
    case (b, e) if b > e => throw new IllegalArgumentException
    case (b, e) if b <= 0 && e >= list.size => list
    case (b, e) => list drop b take e - b
  }

  def sliceTailRec[A](begin: Int, end: Int, list: List[A]): List[A] = (begin, end) match {
    case (b, e) if b > e => throw new IllegalArgumentException
    case (b, e) if b <= 0 && e >= list.size => list
    case (b, e) =>
      @tailrec
      def sliceInternal(acc: List[A], idx: Int, list: List[A]): List[A] = list match {
        case Nil => acc
        case head :: tail if idx >= b && idx < e => sliceInternal(acc :+ head, idx + 1, tail)
        case _ :: tail => sliceInternal(acc, idx + 1, tail)
      }
      sliceInternal(List.empty[A], 0, list)
  }

  /** (P19) Rotate a list N places to the left. */
  def rotate[A](n: Int, list: List[A]): List[A] = n match {
    case 0 => list
    case _ if n > 0 => list.drop(n) ++ list.take(n)
    case _ if n < 0 => list.drop(list.length + n) ++ list.take(list.length + n)
  }

  @tailrec
  def rotate2[A](n: Int, list: List[A]): List[A] = n % list.length match {
    case 0 => list
    case c if c < 0 => rotate2(c + list.length, list)
    case c => (list drop c) ::: (list take c)
  }

  /** (P20) Remove the Kth element from a list. */
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = n match {
    case _ if n >= list.length => throw new IllegalArgumentException
    case _ =>
      def removeAtInternal(acc: (List[A], A), idx: Int, list: List[A]): (List[A], A) = list match {
        case Nil => acc
        case head :: tail if idx == n => removeAtInternal((acc._1, head), idx + 1, tail)
        case head :: tail => removeAtInternal((acc._1 :+ head, acc._2), idx + 1, tail)
      }
      removeAtInternal((Nil, null.asInstanceOf[A]), 0, list)
  }
}
