package edu.knoldus

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    for(i <- 1 until array.length){
      val hold = array(i)
      var j = i-1

      while(j >= 0 && hold < array(j)){
        array(j + 1) = array(j)
        j -= 1
      }
      array(j + 1) = hold
    }
    array
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    for (i <- 0 until array.length-1){
      var min = i
      for (j <- i + 1 until array.length){
        if (array(j) < array(min)){
          min = j
        }
        val temp = array(i)
        array(i) = array(min)
        array(min) = temp
      }
    }
    array
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    var Swaping = false

    for (i <- 0 until array.length - 1)
      if (array(i + 1) < array(i)){
        val temp = array(i)
        array(i) = array(i + 1)
        array(i + 1) = temp
        Swaping = true
      }
      if (Swaping){
        bubbleSort(array)
      }
      else {
        array
      }
    }

}
