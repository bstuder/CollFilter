
/*
 * A Matrix representation mainly used to perform
 * matrix multiplication and LU factorisation
 */
class Matrix(els: List[List[Float]]) {
  val elements: List[List[Float]] = els
  def nRows: Int = elements.length
  def nCols: Int = elements match {
    case Nil => 0
    case x :: xs => x.length
  }

  //Check for well formed matrix
  require(elements.forall(_.length == nCols))

  private def addRows(a: List[Float],
    b: List[Float]): List[Float] =(a, b).zipped map (_ + _)

  private def subRows(a: List[Float],
    b: List[Float]): List[Float] = (a, b).zipped map (_ - _)

  def +(other: Matrix): Matrix = {
    require((other.nRows == nRows) && (other.nCols == nCols))
    new Matrix((elements, other.elements).zipped.map(addRows(_, _)))
  }

  def -(other: Matrix): Matrix = {
    require((other.nRows == nRows) &&
      (other.nCols == nCols))
    new Matrix((elements, other.elements).zipped.map(subRows(_, _)))
  }

  def transpose: Matrix = new Matrix(elements.transpose)

  def *(other: Matrix): Matrix = {
    require(nCols == other.nRows)
    val t = other.transpose
    new Matrix(
      for (row <- elements) yield {
        for (otherCol <- t.elements)
          yield Matrix.dotVectors(row, otherCol)
      })
  }

  // a sligth improvement for the case : this * this.transpose
  def dotTranspose: Matrix = {
    val test = 
      for (row <- elements) yield {
        for (col <- elements)
          yield Matrix.dotVectors(row, col)
      }
      
      new Matrix(test.toList map (row => row.toList))
  }

  // Solve the case : this * x = other with LU decompositon of this
  def LUsolve(other: Matrix): Matrix = {
      require(other.nRows == nCols && other.nCols == 1)
      //val lu = this.LUdecomp
      val lu = this.fastLU
      val s1 = lu._1.diagDownSolve(other)
      val s2 = lu._2.diagUpSolve(s1)
      s2
  }

  /* A faster version of the LU decompostion algorithm.
   * May not be safe for side cases
   */
  def fastLU: (Matrix, Matrix) = {
    require(nCols == nRows)

    var L:IndexedSeq[IndexedSeq[Float]] = IndexedSeq.fill(nRows)(IndexedSeq.fill(nRows)(0f))
    var U:IndexedSeq[IndexedSeq[Float]] = IndexedSeq.fill(nRows)(IndexedSeq.fill(nRows)(0f))

    def parSum(i: Int, j: Int, n: Int): Float = {
      var sum = 0f
      if(n == 0) {
        return 0f
      }
      for(m <- (0 to n-1)) {
        sum += L(i)(m) * U(m)(j)
      }
      sum
    }

    def slvL2(i: Int)(j: Int): Float =
      (elements(i)(j) - parSum(i,j,j)) / U(j)(j)

    def slvU2(i: Int)(j: Int): Float =
      elements(i)(j) - parSum(i,j,i)

    def slvL(i: Int)(j: Int): Float =
      if(i < j) 0 else if(i == j) 1 else slvL2(i)(j)

    def slvU(i: Int)(j: Int): Float =
      if(i > j) 0 else slvU2(i)(j)

    def stepSolve = {
      for(i <- (0 to nRows-1)){
        for(j <- (0 to nRows-1)){
          L = L.updated(i, L(i).updated(j, slvL(i)(j)))
          U = U.updated(i, U(i).updated(j, slvU(i)(j)))
        }
      }
    }

    stepSolve

    def finalize(iseq: IndexedSeq[IndexedSeq[Float]]): List[List[Float]] = {
      iseq.toList map (_.toList)
    }

    (new Matrix(finalize(L)), new Matrix(finalize(U)))
  }

  // using Doolittle LU decomposition
  /*def LUdecomp: (Matrix,Matrix) = {
      require(nCols == nRows)

      var L:List[List[Option[Float]]] = List.fill(nRows)(List.fill(nRows)(None))
      var U:List[List[Option[Float]]] = List.fill(nRows)(List.fill(nRows)(None))

      def parSum(i: Int, j: Int, n: Int): Option[Float] = {
          var sum: Float = 0f
          if(n == 0){
            return Option(sum)
          }
          for(m <- (0 to n-1)) {
             (L(i)(m), U(m)(j)) match {
               case (None,_) | (_,None) => return None
               case (Some(l),Some(u)) => sum += l * u
             }
          }
          Option(sum)
      }
      def slvL2(i: Int)(j: Int): Option[Float] = U(j)(j) match {
         case Some(u) => parSum(i,j,j) match {
            case Some(sum) => Option((elements(i)(j) - sum) / u)
            case None => None
         }
         case None => None
      }

      def slvU2(i: Int)(j: Int): Option[Float] = parSum(i,j,i) match {
         case Some(sum) => Option(elements(i)(j) - sum)
         case None => None
      }

      def slvL(i: Int)(j: Int): Option[Float] = {
         if(i < j)
           Some(0)
         else if(i == j)
           Some(1)
         else
           slvL2(i)(j)
      }

      def slvU(i: Int)(j: Int): Option[Float] = {
         if(i > j)
           Some(0)
         else
           slvU2(i)(j)
      }

      def stepSolve = {
        for(i <- (0 to nRows-1)){
          for(j <- (0 to nRows-1)){
            L = L.updated(i, L(i).updated(j, slvL(i)(j)))
            U = U.updated(i, U(i).updated(j, slvU(i)(j)))
          }
        }
      }

      def isFullySet(lis: List[List[Option[Float]]]): Boolean = {
        lis forall (line => line forall (opt => !opt.isEmpty))
      }

      def finalize(lis: List[List[Option[Float]]]): Matrix = {
        new Matrix(lis map (line => line map (x => x.get)))
      }

      while(!(isFullySet(L) && isFullySet(U))) {
        stepSolve
      }

      (finalize(L),finalize(U))
  }*/

  // Solve the case this * x = other IF this is down diagonal
  private def diagDownSolve(other: Matrix): Matrix = {
    require(other.nRows == nCols && other.nCols == 1)
    for(i <- (0 to nRows-1)) {
        require(elements(i).drop(i+1).forall(_ == 0)) //if the last drop => Nil bug then do a match : case l => l.forall ..... case Nil => true
    }
    var x : List[Float] = List.fill(nRows)(0f)

    def getX(i: Int) = {
      (other.elements(i)(0) - elements(i).zip(x).map(v => v._1 * v._2).reduce(_+_)) / elements(i)(i)
    }
    for(i <- (0 to nRows-1)) {
      x = x.updated(i, getX(i))
    }
    new Matrix(List(x).transpose)
  }

  // Solve the case this * x = other IF this is up diagonal
  private def diagUpSolve(other: Matrix): Matrix = {
    require(other.nRows == nCols && other.nCols == 1)
    for(i <- (0 to nRows-1)) {
        require(elements(i).take(i-1).forall(_ == 0)) //if the last drop => Nil bug then do a match : case l => l.forall ..... case Nil => true
    }
    var x : List[Float] = List.fill(nRows)(0f)

    def getX(i: Int) = {
      (other.elements(i)(0) - elements(i).zip(x).map(v => v._1 * v._2).reduce(_+_)) / elements(i)(i)
    }

    for(i <- (0 to nRows-1)) {
      x = x.updated(nRows-1-i, getX(nRows-1-i))
    }
    new Matrix(List(x).transpose)
  }

  override def toString: String = {
    val rowStrings =
      for (row <- elements)
        yield row.mkString("[", ", ", "]")
    rowStrings.mkString("", "\n", "\n")
  }
}

object Matrix {

  // the identity matrix
  def identity(size: Int): Matrix = weightIdentity(size)(1)

  // the weighted identity matrix (the diagonal contains the coef value)
  def weightIdentity(size: Int)(coef: Float): Matrix = {
    new Matrix(List.range(0,size) map (x => List.fill(size)(0f) updated(x,coef)))
  }

  def dotVectors(a: List[Float], b: List[Float]): Float = {
    (a, b).zipped map(_ * _) reduce (_ + _)
  }
}
