package edu.knoldus
import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def binarySearch(array: Array[Int], elem: Int, initial: Int, last: Int): Int = {
      if (initial > last) return -1
      val mid = initial + (last - initial) / 2
      array(mid) match {
        case i if (i == elem) => mid
        case i if (i > elem) => binarySearch(array, elem, initial, mid - 1)
        case _ => binarySearch(array, elem, mid - 1, last)
      }
    }

    val search = binarySearch(array, elem, initial = 0, last = array.length - 1)
    if (search == (-1))
      false
        else
      true
  }


  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    def linearSearch(elem: Int, array: Array[Int], index: Int = 0): Int = {
        if (array.head.equals(elem)) index
        else if (array.tail.isEmpty) -1
        else linearSearch(elem, array.tail, index +1)
    }
    val search = linearSearch(elem, array, index = 0)
        if (search==(-1))
        false
          else
        true
  }
}