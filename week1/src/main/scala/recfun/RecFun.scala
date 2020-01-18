package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_helper(rem: List[Char], stack: List[Char]): Boolean =
    {
      if (rem.isEmpty)
        if (stack.isEmpty) true
        else false
      else rem.head match {
        case '(' => balance_helper(rem.tail, '(' :: stack)
        case ')' =>
          if (!stack.isEmpty && stack.head.equals('(')) balance_helper(rem.tail, stack.tail)
          else false
        case _ => balance_helper(rem.tail, stack)
      }
    }
    balance_helper(chars, List[Char]())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      def sortedChange(m: Int, c: List[Int]): Int = {
        if (m < 0 || c.isEmpty) 0
        else if (m == 0) 1
        else sortedChange(m - c.head, c) + sortedChange(m, c.tail)
      }
    sortedChange(money, coins.sortWith(_ > _))
  }

}
