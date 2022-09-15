import scala.io.StdIn.readLine
import scala.io.Source

// http://www.macfreek.nl/memory/Letter_Distribution
val freqCharEnglish = Map(
    32  -> 0.18829,//space
    97  -> 0.06532,//a
    98  -> 0.01259,//b
    99  -> 0.02234,//c
    100 -> 0.3283, //d
    101 -> 0.10267,//e
    102 -> 0.01983,//f
    103 -> 0.01625,//g
    104 -> 0.04979,//h
    105 -> 0.05668,//i
    106 -> 0.00097,//j
    107 -> 0.00561,//k
    108 -> 0.03318,//l
    109 -> 0.02027,//m
    110 -> 0.05712,//n
    111 -> 0.06160,//o
    112 -> 0.01504,//p
    113 -> 0.00083,//q
    114 -> 0.04988,//r
    115 -> 0.05317,//s
    116 -> 0.07517,//t
    117 -> 0.02276,//u
    118 -> 0.00796,//v
    119 -> 0.01704,//w
    120 -> 0.00141,//x
    121 -> 0.01428,//y
    122 -> 0.00051 //z
)

/** Translates an ASCII character to the corresponding hexa value
   *
   *  @param hex_char an ASCII character 
   *  @return an hexa value
   */
def ASCIIToHexa(hex_char: Char): Int = hex_char.toInt match 
    case digit  if 48 to 57 contains digit => digit - 48
    case letter if 97 to 102 contains letter => letter - 87
    case _ => throw new Exception("Incorrect hex character (ASCII value must be in [48;57] or [97;102].")

/** Translates two hexa values into the corresponding byte value
   *
   *  @param hex1 an hexa value 
   *  @param hex2 an hexa value 
   *  @return a byte value
   */
def getByteFromHexa(hex1: Int, hex2: Int): Int = (hex1 << 4) + hex2

/** Computes the xor of a value represented by two hexa symbols and a byte
   *
   *  @param xorByte a byte value
   *  @param char1 an hexa symbol 
   *  @param char2 an hexa symbol
   *  @return a byte value
   */
def xorChar(xorByte: Int, char1: Char, char2: Char = '0'): Int =
    getByteFromHexa(ASCIIToHexa(char1), ASCIIToHexa(char2)) ^ xorByte

/** Computes the xor of a list of characters and a byte
   *
   *  @param input a list of characters
   *  @param xorByte a byte value
   *  @return a byte value
   */
def xorInput(input: List[Char], xorByte: Int): List[Int] = input match
    case char1 :: char2 :: tail => xorChar(xorByte, char1, char2) :: xorInput(tail, xorByte) 
    case char :: tail => xorChar(xorByte, char) :: xorInput(tail, xorByte) 
    case _ => List()

/** Find the most likely key which was xored with the plaintext
   *
   *  @param input a list of characters
   *  @return the encrypting key
   */
def breakSingleByteXORCipher(input: List[Char]): (Int, Double) =
    val size = input.size / 2
    val metrics =
        for
            byte <- 0 to 255
        yield {
            val xoredString = xorInput(input, byte)
            val distances =
                for 
                    (keys, freq) <- freqCharEnglish
                yield Math.abs(freq * size.asInstanceOf[Double] - countCharacter(xoredString, keys).asInstanceOf[Double])
            (byte, distances.sum)
        }
    metrics.minBy(_._2)

/** Count the number of occurencies of a specific character in a list of characters
   *
   *  @param input a list of characters
   *  @param char the target character
   *  @param count the partial number of occurencies (optional)
   *  @return the number of occurencies
   */
def countCharacter(input: List[Int], char: Int, count: Int = 0): Int =
    input match     
        case x :: tail if x == char || (x == char - 32 && (97 to 123 contains char))  => countCharacter(tail, char, count + 1)
        case _ :: tail => countCharacter(tail, char, count)
        case _ => count

/** Find the xor ciphertext in a file
   *
   *  @param file a file containing lines of potential ciphertexts
   *  @return a tuple of the ciphertext and its key
   */
def breakSingleByteXORCipherFile(file: Source): (List[Char], Int) =
    val potentialPlaintexts =
    for 
        line <- file.getLines()
    yield (breakSingleByteXORCipher(line.toList), line.toList)

    val res = potentialPlaintexts.minBy(_._1._2)
    (res._2, res._1._1)

object Challenge_4 extends App {
    println("File to analyze :")
    val input = readLine()
    val source = Source.fromFile(input)

    val res = breakSingleByteXORCipherFile(source)
    xorInput(res._1, res._2).foreach(char => print(char.toChar))
    source.close()
}