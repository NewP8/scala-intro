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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balance(chars: List[Char], 0)

  def balance(chars: List[Char], cnt: Int): Boolean = chars match {
    case Nil => cnt == 0
    case '(' :: xs => balance(xs, cnt+1)
    case ')' :: xs => if (cnt > 0) balance(xs, cnt-1) else false
    case _ :: xs => balance(xs, cnt)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = tentativi(money,coins,0)

  def tentativi(m:Int, coins: List[Int], acc:Int): Int = {
    if (acc == m) 1
    else {
      if (acc > m) 0
      else {
        coins match {
          case Nil => 0
          case x :: xs => tentativi(m, coins, acc + x) + tentativi(m, xs, acc)
        }
      }
    }
  }
}
