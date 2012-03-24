
import scala.io.Source
import scala.util.matching.Regex
import scala.io.BufferedSource

/* iterator over and Array containing the elements
 * Movies = MovieID::Title::Genres
 * Ratings = UserID::MovieID::Rating::Timestamp 
 */
object DataSetReader {
  
  //format is : UserID::MovieID::Rating::Timestamp

  val separator: Regex = """::""".r

  def apply(path: String): Stream[(Int,Int,Float)] = {
    val src: BufferedSource = Source fromFile path

    // iterator lines by lines
    val iter: Iterator[String] = src getLines
    
    // split iterator and take the first 3 columns
    val list2 = iter map (line => {
      val tmp = separator.split(line).toList
      (tmp(0).toInt, tmp(1).toInt, tmp(2).toFloat)
    })
    
    list2.toStream
  }
}
