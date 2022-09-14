import scala.io.StdIn.readLine

class TermLines (cond: (String => Boolean) = _ != "") extends Iterator[String] { 
  var s = readLine; 
  def hasNext = cond(s); 
  def next = { var r = s; s = readLine; r } 
}

def byteToHexa(value: Int): List[Int] =
    List(toHexa(value / 16), toHexa(value % 16))

def toHexa(value: Int) = value match
    case a if 0 to 9 contains a => a + 48 
    case b if 10 to 15 contains b => b + 87
    case _ => throw new Exception("Incorrect hexa value") 

def xorChar(char: Int, keyByte: Int): List[Int] =
    byteToHexa((char ^ keyByte) & 0xff)

def rotateKey(input: List[Char], key: List[Char]): List[Int] = input match
    case char :: rest => key match
        case head :: keyRest => println(char);xorChar(char.toInt, head.toInt) ++ rotateKey(rest, keyRest :+ head)
        case _ => throw new Exception("Key error")
    case Nil => List()

object Challenge_5 extends App {
    val key = readLine().toList
    val input = (new TermLines).toVector.mkString("\n").toList
    for elem <- rotateKey(input, key) do print(elem.toChar)
    println
}