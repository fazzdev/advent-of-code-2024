package utility

object Extensions {
  implicit class RichInteger(val value: Int) extends AnyVal {
  }

  implicit class RichString(val value: String) extends AnyVal {
    def splitByTwo(regex: String): (String, String) = {
      val Array(value1, value2, _*) = value.split(regex)

      (value1, value2)
    }
  }

  implicit class RichArray(val value: Array[String]) extends AnyVal {
    def remove(index: Int): Array[String] = value.take(index) ++ value.drop(index + 1)

    def middle(): String = value(value.length / 2)

    def zipDropOne(): Array[(String, String)] = value.zip(value.drop(1))

    def swap(index1: Int, index2: Int): Array[String] = {
      val copyArray = value.clone() // to prevent modifying existing array
      val buffer = copyArray(index1)
      copyArray.update(index1, copyArray(index2))
      copyArray.update(index2, buffer)

      copyArray
    }
  }
}
