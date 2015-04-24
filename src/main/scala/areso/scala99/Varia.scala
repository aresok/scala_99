package areso.scala99

object Varia {

  /** One-line XOR */
  def xor(data: String, key: String): String =
    data.foldLeft[(String, Int)](("", 0))((x, c) => (x._1 + c.toInt.^(key(x._2 % key.length).toInt).toChar, x._2 + 1))._1

  implicit class CharPimpUp(c: Char) {
    def xor(c2: Char): Char = (c.toInt ^ c2.toInt).toChar
  }

  /** One-line XOR with casting moxed to implicit class */
  def xor2(data: String, key: String): String =
    data.foldLeft[(String, Int)](("", 0))((x, c) => (x._1 + c.xor(key(x._2 % key.length)), x._2 + 1))._1

}
