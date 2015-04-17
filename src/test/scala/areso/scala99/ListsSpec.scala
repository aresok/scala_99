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

}
