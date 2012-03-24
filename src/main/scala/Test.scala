import Constants._

object Test {
  val path100k = "src/main/resources/ratings100K.dat"
  val path1m = "src/main/resources/ratings1M.dat"
  val path10m = "src/main/resources/ratings10M.dat"
  
  val curr = path100k
    
  val usage = "\nThe arguments are : parallelisation, number of steps," + 
    "number of hidden factors, lambda regulation values \n" +
    "Default values are parallelisation off, steps=1, Nf="+NF+", lambda="+LAMBDA+"\n" +
    "Usage: [par] [steps [Nf] [lambda]]\n" + 
    "examples : \n\trun par 3 10 0.2 --> parallel, 3 steps, 10 hidden factors, lambda=0.2\n" +
    "\trun 10 50 --> serial, 10 steps, 50 hidden factors, lambda="+LAMBDA+"(default)\n" + 
    "\trun par 5 100 --> parallel, 5 steps, 100 hidden factors, lambda="+LAMBDA+"(default)\n" + 
    "\trun 10 0.1--> serial, 10 steps, "+NF+" hidden factor(default), lambda=0.1\n "
  
  def main(arg: Array[String]) {
    if(checkPar(arg))
      startParallel(parseArgs(arg drop 1))
    else 
      startSerial(parseArgs(arg))
  }
  
  def getData = new DataSetInitializer(DataSetReader(curr))
  
  def startParallel(parg: (Int,Int,Float)) = {
    val parAlg = new ParALSAlgorithm(getData,parg._2,parg._3)
    parAlg.stepN(parg._1)
  }
  
  def startSerial(parg: (Int,Int,Float)) = {
    val alg = new ALSAlgorithm(getData,parg._2,parg._3)
    alg.stepN(parg._1)
  }
  
  def checkPar(arg: Array[String]):Boolean = arg.toList match {
    case "par" :: xs => true
    case _ => false
  }
  
  // Parsing the arguments of the run
  def parseArgs(arg: Array[String]):(Int,Int,Float) = arg.toList match {
      case Nil => (1,NF,LAMBDA)
      case n :: Nil => n match {
        case x if isInt(x) => (x.toInt,NF,LAMBDA)
        case _ => println(usage) ; sys.exit(0)
        }
      case n :: nf :: Nil if isInt(n) && isInt(nf) => 
        (n.toInt,nf.toInt,LAMBDA)
      case n :: lam :: Nil if isInt(n) && isFloat(lam) => 
        (n.toInt,NF,lam.toFloat)
      case n :: nf :: lam :: Nil if isInt(n) && isInt(nf) && isFloat(lam) => 
        (n.toInt, nf.toInt, lam.toFloat)
      case _ => println(usage) ; sys.exit(0)
    }
  
  // simple matcher
  def isInt: String => Boolean = _.matches("\\d+")
  def isFloat: String => Boolean = _.matches("\\d+\\.\\d+")
  
  /* a simple test to check the correcteness of LU decomposition*/
  def testLU = {
	 val r1:List[Float] = List(2,-1,1)
	 val r2:List[Float] = List(0,3,-2)
	 val r3:List[Float] = List(1,4,-1)
	 val mat = new Matrix(r1::r2::r3::Nil)
	 val LU = mat.fastLU
	 println(LU._1 * LU._2)
	 println(LU._1 * LU._2 - mat) // this should be ~= null matrix
  }
}
