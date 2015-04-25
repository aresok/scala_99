package areso.scala99

import areso.scala99.Lists._
import org.scalatest.{Matchers, FlatSpec}

class ListsSpec extends FlatSpec with Matchers {

  "last" should "return last element" in {
    last(List(1,1,2,3,5,8)) shouldEqual 8
  }
  it should "throw NoSuchElementException for empty list" in {
    val emptyList = List.empty[Int]
    a [NoSuchElementException] should be thrownBy {
      last(emptyList)
    }
  }

  "penultimate" should "return last but one element" in {
    penultimate(List(1,1,2,3,5,8)) shouldEqual 5
  }
  it should "throw NoSuchElementException for empty list" in {
    a [NoSuchElementException] should be thrownBy {
      penultimate(List.empty[Int])
    }
    a [NoSuchElementException] should be thrownBy {
      penultimate(List(1))
    }
  }

  "nth" should "return nth element" in {
    nth(2, List(1,1,2,3,5,8)) shouldEqual 2
  }
  it should "throw NoSuchElementException" in {
    a [NoSuchElementException] should be thrownBy {
      nth(7, List(1,1,2,3,5,8))
    }
    a [NoSuchElementException] should be thrownBy {
      nth(0, List.empty[Int])
    }
  }

  "length" should "return the list size" in {
    Lists.length(List(1,1,2,3,5,8)) shouldEqual 6
  }
  it should "return 0 for empty list" in {
    Lists.length(List.empty[Int]) shouldEqual 0
  }

  "reverse" should "return reversed list" in {
    reverse(List(1,1,2,3,5,8)) shouldEqual List(8,5,3,2,1,1)
  }
  it should "return empty list" in {
    reverse(List.empty[Int]) shouldEqual List.empty[Int]
  }

  "isPalindrome" should "return true for even-sized palindrome" in {
    isPalindrome(List[Int](1,2,3,2,1)) shouldBe true
  }
  it should "return true for odd-sided palindrome" in {
    isPalindrome(List[Int](1,2,3,3,2,1)) shouldBe true
  }
  it should "return fasle for non-palindrome" in {
    isPalindrome(List[Int](1,1,2,3,5,8)) shouldBe false
  }

  "flatten" should "flat not-flatten list" in {
    flatten(List[Any](1,1,3,List(5,8))) shouldEqual List(1,1,3,5,8)
  }
  it should "flat deep not-flatten list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1,1,2,3,5,8)
  }
  it should "flat second deep not-flatten list" in {
    flatten(List(1,List(1,List(2,List(3,List(5,List(8))))))) shouldEqual List(1,1,2,3,5,8)
  }
  it should "do nothing for already flat list" in {
    flatten(List(1,1,2,3,5,8)) shouldEqual List(1,1,2,3,5,8)
  }

  "flatten2" should "flat not-flatten list" in {
    flatten2(List[Any](1,1,3,List(5,8))) shouldEqual List(1,1,3,5,8)
  }
  it should "flat deep not-flatten list" in {
    flatten2(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1,1,2,3,5,8)
  }
  it should "flat second deep not-flatten list" in {
    flatten2(List(1,List(1,List(2,List(3,List(5,List(8))))))) shouldEqual List(1,1,2,3,5,8)
  }
  it should "do nothing for already flat list" in {
    flatten2(List(1,1,2,3,5,8)) shouldEqual List(1,1,2,3,5,8)
  }

  "compress" should "compress list with duplicate consequent entries" in {
    compress(List(1,1,2,3,5,8)) shouldEqual List(1,2,3,5,8)
  }
  it should "compres larger list with duplicate consequent entries" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }
  it should "not modify a list with unique entries" in {
    compress(List(1,2,3)) shouldEqual List(1,2,3)
  }

  "pack" should "pack duplicate consequent elements into sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }
  it should "pack non-duplicate elements into sublists" in {
    pack(List(1,2,3)) shouldEqual List(List(1), List(2), List(3))
  }
  it should "put empty sublist into empty list" in {
    pack(List.empty[Int]) shouldEqual List(List.empty[Int])
  }

  "encode" should "encode the list" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }
  it should "put empty sublist inot empty listy" in {
    encode(List.empty[Int]) shouldEqual List.empty[(Int,Int)]
  }

  "encodeModifed" should "encode list modified" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "decode" should "decode list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldEqual List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "encodeDirect" should "encode the list" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }
  it should "put empty sublist into empty listy" in {
    encode(List.empty[Int]) shouldEqual List.empty[(Int,Int)]
  }

  "duplicate" should "duplicate the list" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }
  it should "do nothing for empty list" in {
    duplicate(List.empty[Int]) shouldEqual List.empty[Int]
  }

  "duplicateN" should "duplicate each element N times" in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }
  it should "return empty list for 0 times" in {
    duplicateN(0, List(1,2,3)) shouldEqual List.empty[Int]
  }
  it should "return empty list for empty list" in {
    duplicateN(3, List.empty[Int]) shouldEqual List.empty[Int]
  }

  "drop" should "drop every 3rd element from list" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }
  it should "do nothing for 0 value of n param" in {
    drop(0, List(1,2,3)) shouldEqual List(1,2,3)
  }
  it should "drop all elements for 1 value of n param" in {
    drop(1, List(1,2,3)) shouldEqual List.empty[Int]
  }
  "dropFunctional" should "drop every 3rd element from list" in {
    dropFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }
  it should "do nothing for 0 value of n param" in {
    dropFunctional(0, List(1,2,3)) shouldEqual List(1,2,3)
  }
  it should "drop all elements for 1 value of n param" in {
    dropFunctional(1, List(1,2,3)) shouldEqual List.empty[Int]
  }

  "split" should "split list into two" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }
  it should "make full and empty list if cutpoint exceeds origin list size" in {
    split(12, List(1,2,3)) shouldEqual (List(1,2,3), List.empty[Int])
  }
  it should "make empty and full list if cutpoint is 0" in {
    split(0, List(1,2,3)) shouldEqual (List.empty[Int], List(1,2,3))
  }
  "splitFunctional" should "split list into two" in {
    splitFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }
  it should "make full and empty list if cutpoint exceeds origin list size" in {
    splitFunctional(12, List(1,2,3)) shouldEqual (List(1,2,3), List.empty[Int])
  }
  it should "make empty and full list if cutpoint is 0" in {
    splitFunctional(0, List(1,2,3)) shouldEqual (List.empty[Int], List(1,2,3))
  }

  "slice" should "extract a slice from list" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g)
  }
  it should "return whole list for indexes from 0 to end of list" in {
    slice(0, 5, List('a, 'b, 'c, 'd, 'e)) shouldEqual List('a, 'b, 'c, 'd, 'e)
  }
  it should "throw IllegalArgumentException for switched indexes" in {
    a [IllegalArgumentException] should be thrownBy slice(10, 3, List('a))
  }

  "sliceTailRec" should "extract a slice from list" in {
    sliceTailRec(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g)
  }
  it should "return whole list for indexes from 0 to end of list" in {
    sliceTailRec(0, 5, List('a, 'b, 'c, 'd, 'e)) shouldEqual List('a, 'b, 'c, 'd, 'e)
  }
  it should "throw IllegalArgumentException for switched indexes" in {
    a [IllegalArgumentException] should be thrownBy sliceTailRec(10, 3, List('a))
  }

  "rotate" should "rotate a list right for positive value" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  }
  it should "rotate a list left for negative value" in {
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }
  it should "do nothing for 0 value" in {
    rotate(0, List(1,2,3)) shouldEqual List(1,2,3)
  }

  "rotate2" should "rotate a list right for positive value" in {
    rotate2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  }
  it should "rotate a list left for negative value" in {
    rotate2(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }
  it should "rotate a list when N is greater than list size" in {
    rotate2(4, List('a, 'b, 'c)) shouldEqual List('b, 'c, 'a)
  }
  it should "do nothing for 0 value" in {
    rotate2(0, List(1,2,3)) shouldEqual List(1,2,3)
  }

  "removeAt" should "remove element at specified position" in {
    removeAt(1, List('a, 'b, 'c, 'd)) shouldEqual (List('a, 'c, 'd),'b)
  }
  it should "throw IllegalArgumentException if N is not in the bounds" in {
    a [IllegalArgumentException] should be thrownBy removeAt(10, List(1,2,3))
  }

  "insertAt" should "insert element at specified position" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }
  it should "insert element at the end if index is behind list size" in {
    insertAt(5, 10, List(1,2,3)) shouldEqual List(1,2,3,5)
  }
  it should "insert element at 0 index is negative" in {
    insertAt(5, -10, List(1,2,3)) shouldEqual List(5,1,2,3)
  }
  "insertAt2" should "insert element at specified position" in {
    insertAt2('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }
  it should "insert element at the end if index is behind list size" in {
    insertAt2(5, 10, List(1,2,3)) shouldEqual List(1,2,3,5)
  }
  it should "insert element at 0 index is negative" in {
    insertAt2(5, -10, List(1,2,3)) shouldEqual List(5,1,2,3)
  }

  "range" should "create list with specified range" in {
    range(4,9) shouldEqual List(4, 5, 6, 7, 8, 9)
  }
  it should "create single element list wih the same start and end values" in {
    range(3,3) shouldEqual List(3)
  }
  it should "create reverse range" in {
    range(9,4) shouldEqual List(9, 8, 7, 6, 5, 4)
  }

}
