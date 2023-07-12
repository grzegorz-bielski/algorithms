package algos.katas

import scala.collection.mutable.Stack

object Utils:
  def findOdd(xs: Seq[Int]): Int =
    xs.groupMapReduce(identity)(_ => 1)(_ + _).find(_._2 % 2 != 0).map(_._1).get

  def vowelCount(inputStr: String): Int =
    val vowels = Seq("a", "e", "i", "o", "u")

    inputStr
      .split("")
      .nn
      .foldLeft(0)((acc, curr) => if vowels.contains(curr) then acc + 1 else acc)

  // https://leetcode.com/problems/valid-parentheses/
  type Paren = "(" | ")" | "{" | "}" | "[" | "]"

  def balancedParentheses(str: String): Boolean =
    val stack = Stack[Paren]()
    val onParen = (str: Paren) => (stack.pop() == str)

    str
      .split("")
      .nn
      .foldLeft(false)((acc, curr) =>
        curr match
          case c @ ("(" | "{" | "[") =>
            stack.push(c)
            acc

          case ")" => onParen("(")
          case "]" => onParen("[")
          case "}" => onParen("{")
          case _   => false
      ) && stack
      .popAll()
      .find({
        case "(" | "{" | "[" => true
        case a               => false
      })
      .isEmpty
