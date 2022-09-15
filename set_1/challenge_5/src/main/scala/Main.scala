import scala.io.StdIn.readLine

// from https://stackoverflow.com/a/64637443
class TermLines (cond: (String => Boolean) = _ != "") extends Iterator[String] { 
  var s = readLine; 
  def hasNext = cond(s); 
  def next = { var r = s; s = readLine; r } 
}

/** Turn an arbitrary byte into a list of two hexa symbols
   *
   *  @param byte a byte value
   *  @return a list of two hexa symbols
   */
def byteToHexaList(byte: Int): List[Int] =
    List(HexaToASCII(byte / 16), HexaToASCII(byte % 16))

/** Translates a hexa value to the corresponding ASCII value
   *
   *  @param hex_value an hex value
   *  @return an ASCII value
   */
def HexaToASCII(hex_value: Int): Int = hex_value match
    case digit if 0 to 9 contains digit => digit + 48
    case letter if 10 to 15 contains letter => letter + 87
    case _ => throw new Exception("Incorrect ASCII value, can't convert it to hexa")

/** Computes the xor of a character and a byte
   *
   *  @param xorByte a byte value
   *  @param char an hexa symbol
   *  @return a list of two hexa values
   */
def xorCharToHexa(xorByte: Int, char: Int): List[Int] =
    byteToHexaList((char ^ xorByte))

/** Apply a rotating xor key to a list of bytes
   *
   *  @param input a list of bytes to encrypt
   *  @param key a list of bytes
   *  @return the list of xored bytes
   */
def rotatingKeyXor(input: List[Char], key: List[Char]): List[Int] = (input, key) match
    case (char :: tail, keyHead :: keyTail) => xorCharToHexa(keyHead.toInt, char.toInt) ++ rotatingKeyXor(tail, keyTail :+ keyHead)
    case (_, keyHead :: keyTail) => List()
    case (_, _) => throw new Exception("Key error")

object Challenge_5 extends App {
    println("Input Key :")
    val key = readLine().toList
    println("Input Text (empty line to end input) :")
    val input = (new TermLines).toVector.mkString("\n").toList
    rotatingKeyXor(input, key).foreach(char => print(char.toChar))
    println
}