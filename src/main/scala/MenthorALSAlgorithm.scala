import menthor.akka._
import akka.actor.Actor.actorOf
import scala.collection.mutable.{ArrayBuffer, Ctrie}
import Matrix._
import scala.math.sqrt
import Constants._

/* User Vertex */
case class usrVertex(override val label: String, uId: Int, uRatings: Ctrie[Int, Float]) extends Vertex[DenseVector](label, ArrayBuffer.empty) {
  val id = uId
  val ratings = uRatings
  
  def update(superstep: Int, incoming: List[Message[DenseVector]]): Substep[DenseVector] = {
    {
      Nil
    } then {
      val msgTup = for(msg <- incoming) yield msg.source match {
        case movVertex(_ , mId, _) => (mId, ratings(mId), msg.value)
      }
      val mSorted = ArrayBuffer.concat(msgTup) sortWith (_._1 < _._1)
      val mRatings = mSorted map (_._3)
      val matMi = Matrix(mRatings.transpose)
      val Ai = matMi.dotTranspose(mRatings.size * LAMBDA)
      val Rij = mSorted map (_._2)
      val Vi = matMi * Rij
      val ui = Ai.CLsolve(Vi)
      value = ui
      for (neighbor <- neighbors) yield Message(this, neighbor, value)
    }
  }
  
  def sumDelta(): Float = {
    var sum = 0f;
    for (neighbor <- neighbors) neighbor match {
      case movVertex(_, mId, _) => {
        val delta = Matrix.dotVectors(neighbor.value, value) - ratings(mId)
        sum += delta * delta
      }
    }
    sum
  }
}

/* Movie Vertex */
case class movVertex(override val label: String, mId: Int, mRatings: Ctrie[Int, Float]) extends Vertex[DenseVector](label, ArrayBuffer.empty) {
  val id = mId
  val ratings = mRatings
  
  def update(superstep: Int, incoming: List[Message[DenseVector]]): Substep[DenseVector] = {
    {
      if (superstep > 0) {
        val msgTup = for (msg <- incoming) yield msg.source match {
          case usrVertex(_ , uId, _) => (uId, ratings(uId), msg.value)
        }
        val uSorted = ArrayBuffer.concat(msgTup) sortWith (_._1 < _._1)
        val uRatings = uSorted map (_._3)
        val matUi = Matrix(uRatings.transpose)
        val Aj = matUi.dotTranspose(uRatings.size * LAMBDA)
        val Rij = uSorted map (_._2)
        val Vj = matUi * Rij
        val mj = Aj.CLsolve(Vj)
        value = mj
      }
      for (neighbor <- neighbors) yield Message(this, neighbor, value)
    } then {
      Nil
    }
  }
}

class MenthorALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  var g: Graph[DenseVector] = null
  val ga = actorOf({ g = new Graph[DenseVector]; g })
  var dsi: DataSetInitializer = dataSetInit
  var initialM: Ctrie[Int, DenseVector] = new Ctrie()
  
  def init {
    initialM ++= dsi.setUpM(Nf)
  }
  
  def build() = {
    println("[Menthor] Building Graph")
    val vertexMap = new Ctrie[Int, Vertex[DenseVector]]()
    
    // adding user vertices
    for((uId, hashmap) <- dsi.usrToMov) {
      val vertex = new usrVertex("u" + uId, uId, hashmap)
      vertexMap += ((uId, vertex))
      g.addVertex(vertex)
    }
    
    // adding movies vertices and connecting them
    for((mId, hashmap) <- dsi.movToUsr) {
      val vertex = new movVertex("m" + mId, mId, hashmap)
      vertex.value = initialM(mId)
      for((uId, _) <- hashmap) {
        val userVertex = vertexMap(uId)
        // double connect movie <-> user
        userVertex connectTo vertex
        vertex connectTo userVertex
      }
      g.addVertex(vertex)
    }
  }
  
  def setMode() = {
    //g.setOpMode(SingleWorkerMode)
    //g.setOpMode(MultiWorkerMode)
    g.setOpMode(FixedWorkerMode(20))
    //g.setOpMode(IAmLegionMode)
  }
  
  def checkNorm() = {
    println("[Menthor] Collecting values")
    g.synchronized {
      var norm = 0f
      for (v <- g.vertices) v match {
        case vertex: usrVertex => norm += vertex.sumDelta()
        case _ =>
      }
      val rmse = sqrt(norm / dsi.nbrRatings).toFloat
      norm = sqrt(norm).toFloat
      println("\nFrobenius norm : " + norm + " (RMSE : " + rmse + ")" + "\n")
    }
  }
    
  def stepN(iterations: Int) {
    build()
    setMode()
    ga.start()
    g.iterate(iterations * 2)
    checkNorm()
    g.terminate()
  }
}

object MenthorALSAlgorithm {    
  def main(arg: Array[String]) {
    this(DataSetInitializer(DataSetReader(path1m)), arg(1).toInt).stepN(arg(0).toInt)
  }
  
  def apply(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float): MenthorALSAlgorithm = {
    val alg = new MenthorALSAlgorithm(dataSetInit, Nf, lambda)
    alg.init
    alg
  }
  
  def apply(dataSetInit: DataSetInitializer, Nf: Int): MenthorALSAlgorithm = {
    MenthorALSAlgorithm(dataSetInit, Nf, LAMBDA)
  }
  
  def apply(dataSetInit: DataSetInitializer, lambda: Float): MenthorALSAlgorithm = {
    MenthorALSAlgorithm(dataSetInit, NF ,lambda)
  }
  
  def apply(dataSetInit: DataSetInitializer): MenthorALSAlgorithm = {
    MenthorALSAlgorithm(dataSetInit, NF, LAMBDA)
  }

}
