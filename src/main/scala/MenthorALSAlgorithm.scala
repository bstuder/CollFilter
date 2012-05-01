import menthor.akka._
import akka.actor.Actor.actorOf
import scala.collection.mutable.{ArrayBuffer, Ctrie}
import Matrix._
import scala.math.sqrt
import Constants._
import MenthorALSAlgorithm.Data

/* User Vertex */
case class usrVertex(uId: Int, uRatings: Ctrie[Int, Float]) extends Vertex[Data]("User Vertex " + uId, (ArrayBuffer.empty, 0f)) {
  val ratings = uRatings
  
  def update(superstep: Int, incoming: List[Message[Data]]): Substep[Data] = {
    {
      Nil
    } then {
      val msgTup = for(msg <- incoming) yield msg.source match {
        case movVertex(mId, _) => (mId, ratings(mId), msg.value._1)
      }
      val mSorted = ArrayBuffer.concat(msgTup) sortWith (_._1 < _._1)
      val mRatings = mSorted map (_._3)
      val matMi = Matrix(mRatings.transpose)
      val Ai = matMi.dotTranspose(mRatings.size * LAMBDA)
      val Rij = mSorted map (_._2)
      val Vi = matMi * Rij
      val ui = Ai.CLsolve(Vi)
      value = (ui, 0f)
      
      for (neighbor <- neighbors) yield Message(this, neighbor, value)
    } then {
      Nil
    } crunch((d1: Data, d2: Data) => (ArrayBuffer.empty, d1._2 + d2._2)) then {
      Nil
    }
  }
}

/* Movie Vertex */
case class movVertex(mId: Int, mRatings: Ctrie[Int, Float]) extends Vertex[Data]("Movie Vertex " + mId, (ArrayBuffer.empty, 0f)) {
  val ratings = mRatings
  
  def update(superstep: Int, incoming: List[Message[Data]]): Substep[Data] = {
    {
      for (neighbor <- neighbors) yield Message(this, neighbor, value)
    } then {
      Nil
    } then {
      val msgTup = for (msg <- incoming) yield msg.source match {
        case usrVertex(uId, _) => (uId, ratings(uId), msg.value._1)
      }
      val uSorted = ArrayBuffer.concat(msgTup) sortWith (_._1 < _._1)
      val uRatings = uSorted map (_._3)
      val matUi = Matrix(uRatings.transpose)
      val Aj = matUi.dotTranspose(uRatings.size * LAMBDA)
      val Rij = uSorted map (_._2)
      val Vj = matUi * Rij
      val mj = Aj.CLsolve(Vj)
      value = (mj, sumDelta(mj))
      Nil
    } crunch((d1: Data, d2: Data) => (ArrayBuffer.empty, d1._2 + d2._2)) then {
      Nil
    }
  }
  
  def sumDelta(mj: DenseVector): Float = {
    var sum = 0f
    for (neighbor <- neighbors) neighbor match {
      case usrVertex(uId, _) => {
        val delta = Matrix.dotVectors(neighbor.value._1, mj) - ratings(uId)
        sum += delta * delta
      }
    }
    sum
  }
}

/* Monitor Vertex */
case class monitor(nbrRatings: Int) extends Vertex[Data]("Monitor vertex", (ArrayBuffer.empty, 0f)) {
  def update(superstep: Int, incoming: List[Message[Data]]): Substep[Data] = {
    {
      println("[Menthor] Step : " + (1 + superstep / NBRSUBSTEPS))
      Nil
    } then {
      println("[Menthor] Solving U")
      Nil
    } then {
      println("[Menthor] Solving M ")
      Nil
    } then {
      Nil
    } then {
      graph.crunchResult match {
        case Some(result) => {
          val rmse = sqrt(result._2 / nbrRatings).toFloat
          println("[Menthor] RMSE : " + rmse + "\n")
        }
        case None =>
      }
      Nil
    }
  }
}


class MenthorALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  var g: Graph[Data] = null
  val ga = actorOf({ g = new Graph[Data]; g })
  var dsi: DataSetInitializer = dataSetInit
  var initialM: Ctrie[Int, DenseVector] = new Ctrie()
  var nbWorker: Int = NBRWORKERS
  
  def init {
    initialM ++= dsi.setUpM(Nf)
  }
  
  def build() = {
    println("[Menthor] Building Graph")
    val vertexMap = new Ctrie[Int, Vertex[Data]]()
    // adding the rmse monitor
    g.addVertex(new monitor(dsi.nbrRatings))
    
    // adding user vertices
    for((uId, hashmap) <- dsi.usrToMov) {
      val vertex = new usrVertex(uId, hashmap)
      vertexMap += ((uId, vertex))
      g.addVertex(vertex)
    }
    
    // adding movies vertices and connecting them
    for((mId, hashmap) <- dsi.movToUsr) {
      val vertex = new movVertex(mId, hashmap)
      vertex.value = (initialM(mId), 0f)
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
    g.setOpMode(FixedWorkerMode(nbWorker))
    //g.setOpMode(IAmLegionMode)
  }
    
  def stepN(iterations: Int) = {
    build()
    setMode()
    ga.start()
    g.iterate(iterations * NBRSUBSTEPS)
    g.terminate()
  }
}

object MenthorALSAlgorithm {
  type Data = (DenseVector, Float)
      
  def main(arg: Array[String]) {
    val alg = this(DataSetInitializer(DataSetReader(path1m)), arg(1).toInt)
    if(arg.length > 2) alg.nbWorker = arg(2).toInt
    alg.stepN(arg(0).toInt)
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
