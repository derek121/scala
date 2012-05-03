package org.derekbrown.misc

import scala.collection.mutable.ArrayBuffer

/**
 * Find the next N numbers after a starting point made up of the same digits (not including 
 * negative sign). An alternate algorithm in Java is used
 * < a href="https://github.com/derek121/java/tree/master/org/derekbrown/misc">in this
 * GitHub repository<a>, in the NextNumbers* classes.
 */
object NextNumbers extends App {

  def find(input: Int, howMany: Int) : List[Int] = {
    // Convert the positive input number to string, create an ArrayBuffer holding each character 
    // of the string, and convert to List[Char] (e.g., List(2, 1, 3))
    val charsList = Math.abs(input).toString().foldLeft(
        new ArrayBuffer[Char]())((buf, c) => buf += c).toList

    // Create a List[List[Char]] of the permutations of charsList, and convert each component 
    // List[Char] to a String, resulting in List[String] (e.g., List(213, 231, 123, 132, 321, 312)).
    // If input had a 0 in it, the leading 0 in some permutations will be dropped when converting
    // to Int, resulting in numbers with fewer digits than input (violating the goal of the problem,
    // so filter out those entries with leading 0, then convert to a List[Int]
    val positiveList = charsList.permutations.toList.map(_.mkString).
      filter(_.head != '0').map(_.toInt)
    
    // If input was negative, we have to include the negative forms of the list values, too,
    val allList = if (input >= 0) positiveList else positiveList ++ positiveList.map(-_)

    // Then sort, drop up through the starting number, then take the desired amount
    val permsList = allList.sorted.dropWhile(_ <= input).take(howMany)
    permsList
  } // find
  
  val howMany = 3
  
  var start = -49816
  var result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
		
  start = -213
  result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
  
  start = 743386
  result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
  
  start = -987
  result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
  
  // Will end before finding 3 matches
  start = -1000
  result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
  
  start = 987
  result = find(start, howMany)
  println(start + ": " + result.mkString(", "))
}
