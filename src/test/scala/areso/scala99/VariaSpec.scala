package areso.scala99

import areso.scala99.Varia._
import org.scalatest.{Matchers, FlatSpec}

class VariaSpec extends FlatSpec with Matchers {


  "xor" should "xor correctly" in {
    val data = "Ala ma kota"
    val key = "abc"
    val xored = xor(data, key)
    xored should not equal data
    val dexored = xor(xored, key)
    dexored shouldEqual data
  }
  "xor2" should "xor correctly" in {
    val data = "Ala ma kota"
    val key = "abc"
    val xored = xor2(data, key)
    xored should not equal data
    val dexored = xor2(xored, key)
    dexored shouldEqual data
  }
}
