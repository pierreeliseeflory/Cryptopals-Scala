import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer

def ASCIIToHexa = (hex: Char) => hex.toInt match 
    case digit if (digit >= 48 && digit < 58) => digit - 48
    case letter if (letter >= 97 && letter < 103) => letter - 87
    case _ => throw new Exception("Incorrect hex character")

def HexaToASCII = (hex_value: Int) => hex_value match
    case x if (x >= 0 && x < 10) => x + 48
    case x if (x >= 10 && x < 16) => x + 87
    case _ => throw new Exception("Incorrect ASCII value, can't convert it to hexa")

def xorChar(char_1: Char, char_2: Char): Int =
    ASCIIToHexa(char_1) ^ ASCIIToHexa(char_2)

def xorList(input_1: List[Char], input_2: List[Char]): List[Int] = (input_1, input_2) match
    case (char_1 :: rest_1, char_2:: rest_2) => HexaToASCII(xorChar(char_1, char_2)) :: xorList(rest_1, rest_2) 
    case (char_1 :: rest_1, Nil) => throw new Exception("List size don't match")
    case (Nil, char_2 :: rest_2) => throw new Exception("List size don't match")
    case (Nil, Nil) => List()

object Challenge_2 extends App {
    val input_1 = readLine().toList
    val input_2 = readLine().toList

    for char <- xorList(input_1, input_2) do print(char.toChar) 
    println
}