import scala.io.StdIn.readLine

/** Translates an ASCII character to the corresponding hexa value
   *
   *  @param hex_char an ASCII character 
   *  @return an hexa value
   */
def ASCIIToHexa(hex_char: Char): Int = hex_char.toInt match 
    case digit  if 48 to 57 contains digit => digit - 48
    case letter if 97 to 102 contains letter => letter - 87
    case _ => throw new Exception("Incorrect hex character (ASCII value must be in [48;57] or [97;102].")


/** Translates a hexa value to the corresponding ASCII value
   *
   *  @param hex_value an hex value
   *  @return an ASCII value
   */
def HexaToASCII(hex_value: Int): Int = hex_value match
    case digit if 0 to 9 contains digit => digit + 48
    case letter if 10 to 15 contains letter => letter + 87
    case _ => throw new Exception("Incorrect ASCII value, can't convert it to hexa")

/** Computes the xor of 2 hexa symbols
   *
   *  @param char_1 an hexa symbol
   *  @param char_2 an hexa symbol
   *  @return the computed xor
   */
def xorChars(char_1: Char, char_2: Char): Int =
    ASCIIToHexa(char_1) ^ ASCIIToHexa(char_2)

/** Computes the xor of 2 hexa strings
   *
   *  @param string_1 a string of hexa symbols
   *  @param string_2 a string of hexa symbols
   *  @return the list of xor values
   */
def xorList(string_1: List[Char], string_2: List[Char]): List[Int] = (string_1, string_2) match
    case (char_1 :: rest_1, char_2:: rest_2) => HexaToASCII(xorChars(char_1, char_2)) :: xorList(rest_1, rest_2) 
    case (char_1 :: rest_1, Nil) => throw new Exception("List size don't match")
    case (Nil, char_2 :: rest_2) => throw new Exception("List size don't match")
    case (Nil, Nil) => List()

object Challenge_2 extends App {
    println("Input string 1 :")
    val string_1 = readLine()
    println("Input string 2 :")
    val string_2 = readLine()

    xorList(string_1.toList, string_2.toList).foreach(char => print(char.toChar)) 
    println
}