import scala.math

object Main {
  def main(args: Array[String]): Unit = {

  }

  //https://www.hackerrank.com/challenges/fp-update-list/problem
  def f(arr:List[Int]):List[Int] = {
    //arr.map(x => Math.abs(x)) - built-in map solution but, lets implement more fun way:
    def newArrayThing(oldOne:List[Int], newOne:List[Int]): List[Int] = oldOne match {
      case x::xs =>
        if (x < 0) newArrayThing(xs, newOne:+(-x))
        else newArrayThing(xs, newOne:+x)
      case Nil => newOne
    }
    newArrayThing(arr, List())
  }

  //https://www.hackerrank.com/challenges/fp-list-length/problem
  def f(arr:List[Int]):Int =  {
    def accumulator(acc: Int, remainingList: List[Int]): Int = remainingList match {
      case Nil => acc
      case x::xs => accumulator(acc+1, xs)
    }
    accumulator(0, arr)
  }

  //https://www.hackerrank.com/challenges/fp-sum-of-odd-elements
  def f(arr:List[Int]):Int = {
    def accumulator(acc: Int, remainingList: List[Int]): Int = remainingList match  {
      case Nil => acc
      case x::xs => if (x%2!=0) accumulator(acc+x, xs)
      else accumulator(acc, xs)
    }
    accumulator(0, arr)
  }

  //https://www.hackerrank.com/challenges/fp-reverse-a-list/problem
  def f(arr:List[Int]):List[Int] = {
    var list = List()
    def iterate(real:List[Int], reverse:List[Int]): List[Int] = real match {
      case Nil => reverse
      case x::xs =>
        iterate(xs, x::reverse)
    }

    iterate(arr, list)
  }

  //https://www.hackerrank.com/challenges/fp-array-of-n-elements/problem
  def f(num:Int) : List[Int] = {
    val list = (for (i <- 0 until num) yield 1).toList
    return list
  }

  //https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem
  def f(arr:List[Int]):List[Int] = {
    arr.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  }

  //https://www.hackerrank.com/challenges/fp-filter-array/problem
  def f(delim:Int,arr:List[Int]):List[Int] = arr match {
    case Nil => Nil
    case x::xs =>
      if (x < delim) x::f(delim,xs)
      else f(delim, xs)
  }

  //https://www.hackerrank.com/challenges/fp-list-replication/problem
  def f(num:Int,arr:List[Int]):List[Int] = {
    for {x <- arr; y <- 1 to num} yield x
  }

  //https://www.hackerrank.com/challenges/fp-hello-world-n-times/problem
  def f(n:Int): Unit =
    if (n>0) {
      println("Hello World")
      f(n-1)
    }
}