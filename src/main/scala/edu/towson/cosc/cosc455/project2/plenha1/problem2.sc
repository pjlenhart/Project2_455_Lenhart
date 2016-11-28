//This worksheet will include the code for binary operations

// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Patrick Lenhart


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)



def listEdits(pList : List[Boolean], qList : List[Boolean]) : List[Boolean] = {
  pList.size == qList.size match{
    case true => pList
    case false => {
      if(qList.size > pList.size)
        pList
      else
        qList
    }
  }
}


def listEdits2(pList : List[Boolean], qList : List[Boolean]) : List[Boolean] = {
  pList.size == qList.size match{
    case true => pList
    case false => {
      if(qList.size > pList.size)
        qList
      else
        pList
    }
  }
}

// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  remainingBits match{
    case head::tail => {
      head match {
        case _ if(carryBit) => !head::finishBinaryAdd(tail,head)
        case _ if(!carryBit) => remainingBits
      }
    }
    case Nil => {
      carryBit match{
        case true => List(carryBit)
        case false => Nil
      }
    }
  }
}

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  ((pBit ^ qBit) && carryBit || (pBit && qBit))
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  ((pBit ^ qBit) ^ carryBit)
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  if(listEdits(pBits, qBits).isEmpty){
    finishBinaryAdd(listEdits2(pBits,qBits),carryBit)
  }
  else{
    addBits(pBits.head, qBits.head, carryBit)::doBinaryAddition(pBits.tail,qBits.tail,getNextCarryBit(pBits.head,qBits.head,carryBit))
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) = {
  intList.map(elem => if(elem == 0)
                        false
                      else
                        true).reverse
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map(elem => if(elem == true)
                            1
                          else
                            0).reverse
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  convertBooleanListToIntList(doBinaryAddition(convertIntListToBooleanList(pList),
    convertIntListToBooleanList(qList), false).reverse).reverse
}


// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println(binaryAddition(pTest1, qTest1) + "Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")
// Extra Credit workspace

def doBinarySubtraction(pList : List[Int], qList : List[Int]) : List[Int] = {
  binaryAddition(pList, twosComplement(qList))
}

def twosComplement(eList : List[Int]) : List[Int] = {
  val thisList : List[Int] = List(1)
  binaryAddition(convertBooleanListToIntList(convertIntListToBooleanList(eList).
    map(elem => !elem)), thisList)
}

val subPList: List[Int] = List(0,0,0,0,0)
val subQList: List[Int] = List(1,0,1,1,0)

doBinarySubtraction(subPList, subQList)