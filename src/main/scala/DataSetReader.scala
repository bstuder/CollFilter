import scala.io.Source
import scala.util.matching.Regex
import scala.io.BufferedSource

/*
 * Read a DataSet with the followinf format :
 * UserID::MovieID::Rating::Timestamp
 */
object DataSetReader {

  val separator: Regex = """::""".r

  def apply(path: String): Stream[(Int, Int, Float)] = {
    val src: BufferedSource = Source fromFile path

    // iterator lines by lines
    val iter: Iterator[String] = src getLines

    // split iterator and take the first 3 columns
    (iter map splitNCast).toStream
  }

  def splitNCast (str: String): (Int, Int, Float) = {
    val tmp = separator.split(str)
    (tmp(0).toInt, tmp(1).toInt, tmp(2).toFloat)
  }
}
