import benchmark.TicToc
import Constants._

object Bench extends TicToc {
  def main(arg: Array[String]) {
    val curr = arg.toList match {
      case "100k" :: _ => path100k
      case "1m" :: _ => path1m
      case "10m" :: _ => path10m
      case _ => path1m
    }
    
    val data = DataSetInitializer(DataSetReader(curr))
    
    def benchSerial(fact: Int) {
      println("\t\t[Benchmark] Serial, factor : " + fact)
      val alg = ALSAlgorithm(data, fact)
      tic
      alg.stepN(STEPS)
      toc("Ser" + fact)
    }
   
    def benchPar(fact: Int) {
      println("\t\t[Benchmark] Parallel, factor : " + fact)
      val alg = ParALSAlgorithm(data, fact)
      tic
      alg.stepN(STEPS)
      toc("Par" + fact)
    }
    
    def benchMenthor(fact: Int) {
      println("\t\t[Benchmark] Menthor, factor : " + fact)
      val alg = MenthorALSAlgorithm(data, fact)
      tic
      alg.stepN(STEPS)
      toc("Mth" + fact)
    }
    
    def bench(fact: Int) {
      benchSerial(fact)
      benchPar(fact)
      benchMenthor(fact)
      printTimesLog
      println("\n")
    }
    
    bench(50)
    bench(100)
    bench(300)
    bench(500)
    bench(700)
    bench(1000)
  }
}
