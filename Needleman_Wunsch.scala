package DNA_Classification

import java.util.ArrayList

import java.util.List

object Needleman_Wunsch {

    def main(args: Array[String]): Unit = {
    val sequence1: String = "TTGACT"
    val sequence2: String = "TTGATC"
    System.out.print(
      "Given Sequences:\nSequence 1: " + sequence1 + "\nSeqeunce 2: " +
        sequence2)
    System.out.print(
      "\n\nPerforming Global Alignment using Needleman-Wunsch Algorithm.....")
    val obj: Needleman_Wunsch = new Needleman_Wunsch()
    // Step 1
    val intialized_matrix: Array[Array[Int]] = obj.Initialization(
      sequence1,
      sequence2,
      sequence1.length + 1,
      sequence2.length + 1)
    // Step 2 matrix filling
    val final_matrix: Array[Array[Int]] =
      obj.matrixFilling(intialized_matrix, sequence1, sequence2)
    // step 3 TraceBack
    val GlobalAlignment: List[String] =
      obj.traceBack(final_matrix, sequence1, sequence2)
    System.out.print(
      "\nResult:\nSequence 1: " + GlobalAlignment.get(0) + "\nSequence 2: " +
        GlobalAlignment.get(1))
  }

}

  class Needleman_Wunsch {

    val `match`: Int = 1

    val mismatch: Int = -1

    val gap: Int = -2

    def printMatrix(matrix: Array[Array[Int]]): Unit = {
      println("\nScore Matrix: ")
      for (i <- 0 until matrix.length) {
        for (j <- 0 until matrix(i).length) {
          System.out.print(matrix(i)(j) + "\t")
        }
        println()
      }
    }

    def Initialization(sequence1: String,
                       sequence2: String,
                       m: Int,
                       n: Int): Array[Array[Int]] = {
      val matrix: Array[Array[Int]] = Array.ofDim[Int](m, n)
      var temp: Int = 2
      for (i <- 0 until m) {
        temp += gap
        matrix(i)(0) = temp
      }
      temp = 2
      for (j <- 0 until n) {
        temp += gap
        matrix(0)(j) = temp
      }
    // printMatrix(matrix);
      matrix
    }

    def matrixFilling(matrix: Array[Array[Int]],
                      sequence1: String,
                      sequence2: String): Array[Array[Int]] = {
      var left: Int = 0
      var top: Int = 0
      var diag: Int = 0
      for (i <- 1 until matrix.length; j <- 1 until matrix(i).length) {
        left = matrix(i - 1)(j) + gap
        top = matrix(i)(j - 1) + gap
        diag = matrix(i - 1)(j - 1) +
          score(sequence1.charAt(i - 1), sequence2.charAt(j - 1))
        val maxval: Int = Math.max(diag, Math.max(top, left))
        matrix(i)(j) = maxval
      }
      printMatrix(matrix)
      matrix
    }

    def score(a: Char, b: Char): Int = {
      var score: Int = 0
      score = if (a == b) `match` else mismatch
      score
    }

    def traceBack(matrix: Array[Array[Int]], sequence1: String, sequence2: String): List[String] = {
      var aligned_1: String = ""
      var aligned_2: String = ""
      var m: Int = sequence1.length()
      var n: Int = sequence2.length()
      while (m > 0 & n > 0) {
        if (m > 0 & n > 0 & (matrix(m)(n) == matrix(m - 1)(n - 1) + score(sequence1.charAt(m - 1), sequence2.charAt(n - 1)))) {
          aligned_1 = sequence1.charAt(m - 1) + aligned_1;
          aligned_2 = sequence2.charAt(n - 1) + aligned_2;
          m -= 1
          m + 1
          n -= 1;
          //n + 1
        }

        else if (m > 0 & matrix(m)(n) == matrix(m - 1)(n) + gap) {
          aligned_1 = sequence1.charAt(m - 1) + aligned_1
          aligned_2 = "-" + aligned_2;
          m -= 1
          //m+1
        }
        else {
          aligned_1 = "-" + aligned_1
          aligned_2 = sequence2.charAt(n - 1) + aligned_2
          n -= 1
          //n+1

       }
      }
      val GlobalAlignment: List[String] = new ArrayList[String]()
      GlobalAlignment.add(aligned_1)
      GlobalAlignment.add(aligned_2)
      GlobalAlignment

    }

}
