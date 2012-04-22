
object Constants {
  // data set path
  val path100k = "src/main/resources/ratings100K.dat"
  val path1m = "src/main/resources/ratings1M.dat"
  val path10m = "src/main/resources/ratings10M.dat"
  // number of hidden variables
  val NF: Int = 20
  // lambda for ALS regulation
  val LAMBDA: Float = 0.015f
  // max value for the small random range [0,max]
  val MAXRAND: Float = 0.5f

  val incr = 50
  val STEPS = 3
}
