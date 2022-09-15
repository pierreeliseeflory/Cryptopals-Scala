import scala.io.StdIn.readLine

/** Translates an ASCII character to the corresponding hexa value
   *
   *  @param hex_char an ASCII character 
   *  @return an hexa value
   */
def ASCIIToHexa(hex_char: Char): Int = hex_char.toInt match
    case digit if 48 to 57 contains digit => digit - 48
    case letter if 97 to 102 contains letter => letter - 87
    case _ => throw new Exception("Incorrect hex character (ASCII value must be in [48;57] or [97;102].")

/** Translates a Base64 value to the corresponding ASCII code value
   *
   *  @param base64 the Base64 value
   *  @param padding a boolean used to differentiate zero-values 
   *         from padding symbols
   *  @return the ASCII code of the corresponding Base64 value
   */
def Base64ToASCII(base64: Int, padding: Boolean = false): Int = 
    if padding then 
        61
    else
        base64 match
            case uppercase if 0 to 25 contains uppercase => uppercase + 65
            case lowercase if 26 to 51 contains lowercase => lowercase + 71
            case digit if 52 to 61 contains digit => digit - 4 
            case 62 => 43
            case 63 => 47
            case _ => throw new Exception("Incorrect value (0 <= Base_64_value < 64)")   

/** Produces Base64 symbols from hexa symbols
   *
   *  @param value_1 a hexa symbol
   *  @param value_2 a hexa symbol
   *  @param value_3 a hexa symbol
   *  @param padding a boolean used to differentiate zero-values 
   *         from padding symbols
   *  @return a tuple of two Base64 symbols
   */
def hexaToBase64(value_1: Char, value_2: Char = '0', value_3: Char = '0', padding: Boolean = false) : (Int, Int) =
    val hexa_1 = ASCIIToHexa(value_1)
    val hexa_2 = ASCIIToHexa(value_2)
    val hexa_3 = ASCIIToHexa(value_3)
    (Base64ToASCII((hexa_1 << 2) + (hexa_2 / 4)), Base64ToASCII(((hexa_2 % 4) << 4) + hexa_3, padding))

/** Translates a list of hexa symbols into a list of Base64 symbols
   *
   *  @param inputList a list of hexa symbols
   *  @param padding a boolean used to differentiate zero-values 
   *         from padding symbols
   *  @return a list of Base64 symbols values
   */
def iterateTriplesOverList(inputList: List[Char]) : List[Int] = inputList match
    case x :: y :: z :: rest => {
        val (res_1, res_2) = hexaToBase64(x, y, z)
        res_1 :: res_2 :: iterateTriplesOverList(rest)
    }
    case x :: y :: Nil => {
        val (res_1, res_2) = hexaToBase64(x, y)
        res_1 :: res_2 :: List() 
    }
    case x :: Nil => {
        val (res_1, res_2) = hexaToBase64(x, padding=true)
        res_1 :: res_2 :: List() 
    }
    case Nil  => {
        List()
    }

object Challenge_1 extends App {
    println("Input hexa string :")
    val input = readLine().toList
    
    for char <- iterateTriplesOverList(input) do print(char.toChar) 
    println
}