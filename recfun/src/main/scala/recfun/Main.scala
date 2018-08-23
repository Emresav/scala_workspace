package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    // print(balance("just an example".toList)) 
    // print("Different ways: " + countChange(4, List(1,2)) )
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
    	if (c == 0 || c == r) 1
    	else pascal(c-1,r-1) + pascal(c,r-1)
  	}
  /**
   * Exercise 2
   */
	var open: Int = 0
	var closed: Int = 0

    def balance(chars: List[Char]) = {

    	def matchChars(c: Char) = {
    		if(c == ')') closed = closed + 1
    		else if (c == '(') open = open + 1
    	}

    	def balanceIter(chars: List[Char]): Boolean = {
    		if(chars.isEmpty) open == closed
    		else {
    			matchChars(chars.head) 
    			balanceIter(chars.tail) 
    		}
    	}

    	balanceIter(chars) 
    }
  
  /**
   * Exercise 3
   */
   //money: the money, coins: different types, return: how many different ways to give change
   // dynamic programming approach is more appropriate

  def countChange(money: Int, coins: List[Int]): Int = {
    def countRecursion(money: Int, coins: List[Int]): Int =
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        countRecursion(money - coins.head, coins) + countRecursion(money, coins.tail)

    countRecursion(money, coins)
}

  }
