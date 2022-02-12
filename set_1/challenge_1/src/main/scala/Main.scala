import scala.io.StdIn.readLine

def ASCIIToHexa = (hex: Char) => hex.toInt match { 
    case digit if (digit >= 48 && digit < 58) => digit - 48
    case letter if (letter >= 97 && letter < 103) => letter - 87
    case _ => throw new Exception("Incorrect hex character")
}

def Base64ToASCII (value: Int, padding: Boolean = false) = 
    if padding then 
        61
    else
        value match {
            case value if (value >= 0 && value < 26) => value + 65
            case value if (value >= 26 && value < 52) => value + 71
            case value if (value >= 52 && value < 62) => value - 4 
            case 62 => 43
            case 63 => 47
            case _ => throw new Exception("Incorrect value : 0 <= Base 64 < 64")
}

def hexaTripletToBase64Couple(value_1: Int, value_2: Int, value_3: Int, padding: Boolean) : (Int, Int)  = {
    (Base64ToASCII((value_1 << 2) + (value_2 / 4)), Base64ToASCII(((value_2 % 4) << 4) + value_3, padding))
}

def hexaToBase64(value_1: Char, value_2: Char = '0', value_3: Char = '0', padding: Boolean = false) : (Int, Int) = {
    hexaTripletToBase64Couple(ASCIIToHexa(value_1), ASCIIToHexa(value_2), ASCIIToHexa(value_3), padding)
}

def iterateOverList(inputList: List[Char]) : List[Int] = inputList match {
    case x :: y :: z :: rest => {
        val (res_1, res_2) = hexaToBase64(x, y, z)
        res_1 :: res_2 :: iterateOverList(rest)
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
}

@main
def Challenge() : Unit =
    val input = readLine().toList

    for char <- iterateOverList(input) do print(char.toChar) 
    println
end Challenge
