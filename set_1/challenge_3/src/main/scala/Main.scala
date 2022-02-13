import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer

// http://www.macfreek.nl/memory/Letter_Distribution
val freqCharEnglish = Map(
    32 -> 0.18829,//space
    97 -> 0.06532,//a
    98 -> 0.01259,//b
    99 -> 0.02234,//c
    100 -> 0.3283,//d
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
    122 -> 0.00051//z
)

def getHexaValue = (ascii: Char) => ascii.toInt match 
    case digit if (digit >= 48 && digit < 58) => digit - 48
    case letter if (letter >= 97 && letter < 103) => letter - 87
    case _ => throw new Exception("Incorrect hex character")

def getByteFromHexa = (hex1: Int, hex2: Int) => ((hex1 << 4) + hex2) 

def xorChar(xorByte: Int, char1: Char, char2: Char = '0'): Int =
    getByteFromHexa(getHexaValue(char1), getHexaValue(char2)) ^ xorByte 

def xorInput(input: List[Char], xorByte: Int): List[Int] = input match
    case char1 :: char2 :: rest => xorChar(xorByte, char1, char2) :: xorInput(rest, xorByte) 
    case char :: rest => xorChar(xorByte, char) :: xorInput(rest, xorByte) 
    case Nil => List()

def breakSingleByteXORCipher(input: List[Char]): Int=
    var bestMetric = -1.0
    var key = -1
    val size = input.size / 2
    for
        byte <- 0 to 255
    do
        val result = xorInput(input, byte)
        var metric = 0.0
        if sanityCheck(result) then
            for ((key, value) <- freqCharEnglish)
            do
                val count = countCharacter(result, key)
                metric += Math.abs(value * size.asInstanceOf[Double] - count.asInstanceOf[Double])
            if metric < bestMetric || bestMetric < 0 then
                bestMetric = metric
                key = byte
    key   

def sanityCheck(input: List[Int]): Boolean =
    input match
        case x :: rest => x match
            case printable if 32 to 126 contains printable => sanityCheck(rest)
            case _ => false
        case Nil => true

def countCharacter(input: List[Int], char: Int): Int =
    input match     
        case head :: rest => head match
            case x if x == char || (x == char - 32 && char >= 97 && char < 123)  => countCharacter(rest, char) + 1
            case _ => countCharacter(rest, char)
        case Nil => 0

@main def Challenge(): Unit =
    val input = readLine().toList
    val byte = breakSingleByteXORCipher(input)
    for chars <- xorInput(input, byte) do print(chars.toChar)
    println
