package recfun

import recfun.RecFun.balance

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], cnt: Int): Boolean = chars match {
      case Nil => cnt == 0
      case '(' :: xs => balance(xs, cnt+1)
      case ')' :: xs => if (cnt > 0) balance(xs, cnt-1) else false
      case _ :: xs => balance(xs, cnt)
    }
    balance(chars: List[Char], 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeAttempt(coins: List[Int], acc:Int): Int = {
      if (acc == money) 1
      else {
        if (acc > money) 0
        else {
          coins match {
            case Nil => 0
            case x :: xs => countChangeAttempt(coins, acc + x) + countChangeAttempt(xs, acc)
          }
        }
      }
    }
    countChangeAttempt(coins,0)
  }
}
