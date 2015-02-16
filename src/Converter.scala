/**
 * Created by danielgoldin on 2/16/15.
 */
object Converter {
  def charToInt(c:Char) =
    c match {
      case x if x >= '0' && x <= '9' => x - '0'
      case x if x >= 'A' && x <= 'Z' => x - 'A' + 10
      case x if x >= 'a' && x <= 'z' => x - 'a' + 10
      case _ => 0
    }

  def hexToBigInt(s:String) =
    s.toCharArray.map(charToInt).foldLeft(BigInt(0))((a, b) => 16 * a + b)

  def intToBase64(i:Int) =
    i match {
      case x if x >= 0 && x <= 25 => (x + 'A').toChar
      case x if x >= 26 && x <= 51 => (x - 26 + 'a').toChar
      case x if x >= 52 && x <= 61 => (x - 52 + '0').toChar
      case 62 => '+'
      case 63 => '/'
    }

  def hexToBase64(s:String) = {
    var n = hexToBigInt(s)
    var r = ""
    while (n > 0) {
      val c = (n % 64).toInt
      n = (n - n % 64)/64

      r = intToBase64(c) + r
    }
    r
  }

  def main(args: Array[String]) {
    val hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val base64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    val base64Test = hexToBase64(hex)
    println(base64 == base64Test)
  }
}
