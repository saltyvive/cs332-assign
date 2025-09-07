package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], cnt: Int) : Int = {
      if(!chars.isEmpty){
        if(cnt < 0) cnt
        else if(chars.head == '(') check(chars.tail, cnt + 1)
        else if(chars.head == ')') check(chars.tail, cnt - 1)
        else check(chars.tail, cnt)
      }
      else cnt
    }
    if(check(chars, 0) == 0) true
    else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def cnt_amount(money: Int, coins: List[Int], amount: Int): Int = {
      if(coins != Nil){
        if(money > 0) cnt_amount(money, coins.tail, amount) + cnt_amount(money - coins.head, coins, amount)
        else if(money == 0) amount + 1
        else amount
      }
      else amount
    }
    cnt_amount(money, coins, 0)
  }
}
