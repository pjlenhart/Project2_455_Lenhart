//Chinese and English number operator

val chinese: List[String] = List("ling","yi","er","san","si","wu","liu",
  "qi","ba","jiu","shi")
val english: List[String] = List("zero","one","two","three","four","five","six",
  "seven","eight","nine","ten")

//converts to numbers, will give -1 if not legal
def convertToNumbers(word : String) : Int = {
  word.toLowerCase match{
    case "ling" => 0
    case "zero" => 0
    case "yi" => 1
    case "one" => 1
    case "er" => 2
    case "two" => 2
    case "san" => 3
    case "three" => 3
    case "si" => 4
    case "four" => 4
    case "wu" => 5
    case "five" => 5
    case "liu" => 6
    case "six" => 6
    case "qi" => 7
    case "seven" => 7
    case "ba" => 8
    case "eight" => 8
    case "jiu" => 9
    case "nine" => 9
    case "shi" => 10
    case "ten" => 10
    case _ => -1
  }
}

def translate(wordList : List[String]) : List[Int] ={
  wordList.map(elem => convertToNumbers(elem))
}

def translation(wordList : List[Int]) : Unit = {
  if(!wordList.isEmpty){
    print(wordList.head + " ")
    translation(wordList.tail)
  }
}

def addition(wordList : List[Int]) : Int ={
  wordList match{
    case Nil => 0
    case num::tail => num + addition(tail)
  }
}

//for purposes of printing it out correctly
def addition2(wordList : List[Int]) : Unit = {
  wordList.isEmpty match{
    case true => {
      print(wordList.head)
      addition2(wordList.tail)
    }
    case false => {
      wordList.length match{
        case 1 => {
          print(wordList.head)
        }
        case _ => {
          print(wordList.head + " + ")
          addition2(wordList.tail)
        }
      }
    }
  }
}

def multiplication(wordList : List[Int]) : Int = {
  wordList match{
    case Nil => 1
    case num::tail => num*multiplication(tail)
  }
}

//for purposes of printing it out correctly
def multiplication2(wordList : List[Int]) : Unit = {
  wordList.isEmpty match{
    case true => {
      print(wordList.head)
      multiplication2(wordList.tail)
    }
    case false => {
      wordList.length match{
        case 1 => {
          print(wordList.head)
        }
        case _ => {
          print(wordList.head + " * ")
          multiplication2(wordList.tail)
        }
      }
    }
  }
}

def go(wordList : List[String]) = {
  var newList : List[Int] = translate(wordList)
  var theList : List[Int] = newList.filter(_ > -1)
  print("Translate: ")
  translation(theList)
  println()
  print("Addition: ")
  addition2(theList)
  print(" = ")
  print(addition(theList))
  println()
  print("Multiplication: ")
  multiplication2(theList)
  print(" = ")
  print(multiplication(theList))
  println()
}

//Test case provided by project description
go(List("yi", "josh", "three","si"))