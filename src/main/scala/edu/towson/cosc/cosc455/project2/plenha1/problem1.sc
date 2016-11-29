///Problem 1 - Prime numbers, twin primes, goldbach conjecture, etc.



def prime(n : Int) : Boolean ={
  var input = 3
  isPrime(n, input) match{
    case true => true
    case false => false
  }
}

// Helper method to test for divisibility of primes, dont want
// to test uncessary cases
def isPrime(n : Int, m : Int) : Boolean = {
  var input = m
  n match{
    case 0 => false
    case 1 => true
    case 2 => true
    case _ => {
      n % m match{
        case 0 => {
          if (n == input)
            true
          else
            false
        }
        case _ => input += 1
          isPrime(n, input)
      }
    }
  }
}

//test cases for prime
prime(11) //should be true
prime(12) //should be false

def twinPrimes(n : Int, m : Int) : Boolean ={
  n - m match{
    case 2 =>
      if(prime(n) && prime(m))
        true
      else
        false
    case -2 =>
      if(prime(n) && prime(m))
        true
      else
        false
    case _ => false
  }
}

//test cases for twin primes
twinPrimes(41,43) //should be true
twinPrimes(43,47) //should be false

def twinPrimesList(n : Int) : List[Int] = {
  buildTwins(twinsList(n).reverse)
}

def buildTwins(primeList : List[Int]) : List[Int] ={
  primeList match{
    case Nil => Nil
    case num::List() => List(num)
    case num::tail if(num == tail.head) => buildTwins(tail)
    case num::tail => num::buildTwins(tail)
  }
}

def twinsList(num : Int) : List[Int] = {
  case 3 => Nil
  case _ =>{
    if(prime(num) && prime(num-2))
      num::num-2::twinsList(num-2)
    else
      twinsList(num-1)
  }
}

//test case for twinPrimesList
twinPrimesList(50) //should output what the assignment has

def goldbach(x : Int) = x match{
  case x if(x <= 2) => print("Must be greater than 2 to fit conjecture")
  case x if(x % 2 == 1) => print("Conjecture requires even number")
  case x if(x % 2 == 0) => {
    var ex = 1
    assisstGoldbach(x , ex)
  }
}

def assisstGoldbach(x : Int, y : Int): Unit = {
  var n1 = x
  var n2 = y
  y match{
    case 2 => assisstGoldbach(n1, n2+1)
    case y if(y == x) =>
    case y if(prime(x-y)) => {
      print(y + " + " + (x-y) + " = " + x)
    }
    case _ => assisstGoldbach(n1,n2+1)
  }
}

//goldbach test case
goldbach(28) //should output "23 + 5 = 28"

//For whatever reason, Goldbach wont run in this Worksheet but I ran it
//on an online compiler it was successful