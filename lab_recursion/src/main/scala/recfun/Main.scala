package recfun

import scala.annotation.tailrec
import scala.util.{Try,Success,Failure}

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(calcExpression(6))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], open: Int): Boolean = {
        if (chars.isEmpty) open == 0
        else if (open < 0) false
        else if (chars.head == '(') loop(chars.tail, open + 1)
        else if (chars.head == ')') loop(chars.tail, open - 1)
        else loop(chars.tail, open)
      }

      loop(chars, 0)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

  /**
   * Exercise 4
   */
    def calcExpression(x: Int): Int = {
      @tailrec
      def iterateFact(x: Int, result: Int = 1): Int =
        if (x == 1) result
        else iterateFact(x - 1, result * x)

      if (x == 0 || x == 1) throw new IllegalArgumentException("function not defined on [0, 1]")
      else if (x < 0) 0
      else iterateFact(x)
    }
  }
