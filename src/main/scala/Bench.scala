import Constants._

object Bench extends TicToc {
  val path100k = "src/main/resources/ratings100K.dat"
  val path1m = "src/main/resources/ratings1M.dat"
  val path10m = "src/main/resources/ratings10M.dat"
  
  def main(arg: Array[String]) {
    val curr = arg.toList match {
      case "100k" :: _ => path100k
      case "1m" :: _ => path1m
      case "10m" :: _ => path10m
      case _ => path100k
    }
    val data = new DataSetInitializer(DataSetReader(curr))
    
    def benchSerial(fact: Int) {
      println("\t\t[Benchmark] Serial factor : " + fact)
      val alg = new ALSAlgorithm(data, fact)
      tic
      alg.stepN(STEPS)
      toc("Ser" + fact)
    }
   
    def benchPar(fact: Int) {
      println("\t\t[Benchmark] Parallel factor : " + fact)
      val alg = new ParALSAlgorithm(data, fact)
      tic
      alg.stepN(STEPS)
      toc("Par" + fact)
    }
    
    def bench(fact: Int) {
      benchSerial(fact)
      benchPar(fact)
      printTimesLog
    }
    
    bench(10)
    bench(30)
    bench(50)
    bench(100)
    //test(200)
    //test(500)
  }
}
