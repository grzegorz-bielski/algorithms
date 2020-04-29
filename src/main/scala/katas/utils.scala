package katas

import scala.collection.mutable.Stack

// import scala.collection.immutable.Stack

object Utils {
  def findOdd(xs: Seq[Int]): Int =
    xs.groupMapReduce(identity)(_ => 1)(_ + _).find(_._2 % 2 != 0).map(_._1).get

  def vowelCount(inputStr: String): Int = {
    val vowels = Seq("a", "e", "i", "o", "u")
    inputStr
      .split("")
      .foldLeft(0)((acc, curr) => if (vowels.contains(curr)) acc + 1 else acc)
  }

  // https://leetcode.com/problems/valid-parentheses/
  def balancedParentheses(str: String): Boolean = {
    val stack = Stack[String]()
    val onParen = (str: String) => (stack.pop() == str)

    str
      .split("")
      .foldLeft(false)((acc, curr) =>
        curr match {
          case "(" | "{" | "[" => { stack.push(curr); acc }
          case ")"             => onParen("(")
          case "]"             => onParen("[")
          case "}"             => onParen("{")
        }
      ) && stack
      .popAll()
      .find({
        case "(" | "{" | "[" => true
        case a               => false
      })
      .isEmpty
  }

}
