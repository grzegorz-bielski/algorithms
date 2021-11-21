package search

// https://leetcode.com/problems/first-bad-version

// setup
class VersionControl:
  def isBadVersion(n: Int): Boolean = ???

class Solution extends VersionControl:
  def firstBadVersion(n: Int): Int = 
    @annotation.tailrec
    def go(min: Int, max: Int): Int = 
      if min == max
      then min 
      else 
        // prevents integer overflow
        val mid = min + (max - min) / 2
        if isBadVersion(mid)
        then go(min, mid)
        else go(mid + 1, max)
    
    go(1, n)
end Solution
    
