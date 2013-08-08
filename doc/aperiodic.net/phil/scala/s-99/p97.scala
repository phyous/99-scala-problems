// P97 (**) Sudoku.
//     Sudoku puzzles go like this:
//
//     Problem statement                Solution
//     
//     .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
//             |         |                      |         |        
//     6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
//             |         |                      |         |        
//     5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
//     --------+---------+--------      --------+---------+--------
//     3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
//             |         |                      |         |        
//     .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
//             |         |                      |         |        
//     .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
//     --------+---------+--------      --------+---------+--------
//     1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
//             |         |                      |         |        
//     .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
//             |         |                      |         |        
//     2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
//   
//
//     Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
//     column, as well as to one single 3×3 square (which we call "square" for
//     short).  At the beginning, some of the spots carry a single-digit number
//     between 1 and 9.  The problem is to fill the missing spots with digits
//     in such a way that every number between 1 and 9 appears exactly once in
//     each row, in each column, and in each square.

// This is a very simple, functional-style implementation.  It uses mutable
// Arrays because they're the only O(1) access structure that Scala currently
// has.  (An O(1) access, O(1) update, immutable data structure might be added
// to Scala 2.8.)  The updates are all done functionally, which results in a
// lot of copying of data.  Hopefully, this will become more efficient in
// Scala 2.8.
//
// Anyway, the approach is pretty simple.  A sudoku board is represented as an
// array of Eithers.  Left elements represent solved cells, while Right
// elements contain the set of possibilities for their cell.  Solving the
// board consists of trying possibilities for each unsolved cell until either
// a solution is found or a contradiction (in the form of an empty Right set)
// is found.
//
// The code is pretty flexible.  The sudoku board can hold any type of data,
// although string2Board generates the traditional values 1..9.  Likewise,
// the board can be any size, but it will only work properly if the array
// passed in has a length that is a fourth power.

class SudokuBoard[T](aboard: Array[Either[T,Set[T]]]) {
  type Board = Array[Either[T,Set[T]]]
  val width = Math.sqrt(aboard.length)
  val squareWidth = Math.sqrt(width)
  val board: Board = cleanBoard(aboard)

  override def toString = {
    def mkSep = 
      List.make(squareWidth, List.make(squareWidth * 3, '-').mkString("")).mkString("+").substring(1, width * 3 + squareWidth - 2)
    def chunk[U](n: Int, seq: Seq[U]): Seq[Seq[U]] =
      (0 until seq.length by n).toList.zip((n to seq.length by n).toList).map {
        case (start, end) => seq.slice(start, end)
      }
    chunk(width * squareWidth, board.map {
      case Left(n) => n.toString
      case _       => "."
    }).map {
      chunk(width, _).map {
        chunk(squareWidth, _).map {
          _.mkString("  ")
        }.mkString(" | ")
      }.mkString("\n")
    }.mkString("\n" + mkSep + "\n")
  }

  private def removeFromRow(row: Int, n: T, b: Board): Board =
    removeFromIndices((row * width) to ((row + 1) * width - 1), n, b)
  private def removeFromCol(col: Int, n: T, b: Board): Board =
    removeFromIndices(col until width * width by width, n, b)
  private def removeFromSquare(square: Int, n: T, b: Board): Board = {
    val start = (square / squareWidth) * (width * squareWidth) + square % squareWidth * squareWidth
    val indices = Stream.from(start, width).take(squareWidth).flatMap(Stream.from(_).take(squareWidth))
    removeFromIndices(indices, n, b)
  }
  private def removeFromIndices(is: Seq[Int], n: T, brd: Board): Board =
    is.foldLeft(brd) {(b, i) =>
      if (b(i).isRight) b(i) = Right(b(i).right.get - n)
      b
    }
  private def cleanBoard(brd: Board): Board =
    brd.zipWithIndex.foldLeft(brd) {
      case (b, (Left(n), i)) =>
        removeFromSquare(i / width / squareWidth * squareWidth + i % width / squareWidth, n,
                         removeFromRow(i / width, n, 
                                       removeFromCol(i % width, n, b)))
      case (b, _) => b
    }
  def setCell(row: Int, col: Int, value: T): SudokuBoard[T] = {
    val newBoard = board.map(c => c)
    newBoard(row * width + col) = Left(value)
    new SudokuBoard(newBoard)
  }
}

object SudokuBoard {
  implicit def string2Board(s: String): SudokuBoard[Int] = {
    val boardArr: Array[Either[Int,Set[Int]]] = new Array(s.length)
    for (i <- 0 until s.length; c = s(i); n = c - '0') {
      boardArr(i) = if (1 <= n && n <= 9) Left(n)
                    else Right(Set() ++ (1 to 9))
    }
    new SudokuBoard(boardArr)
  }
  final def solve[T](b: SudokuBoard[T]): Option[SudokuBoard[T]] = 
    b.board.zipWithIndex.find(_._1.isRight) match {
      case None => Some(b)
      case Some((Right(ns), _)) if ns.isEmpty => None
      case Some((Right(ns), i)) => ns.projection.map {n =>
        solve(b.setCell(i / b.width, i % b.width, n))
      }.find(_.isDefined) match {
        case None => None
        case Some(ans) => ans
      }
      case _ => throw new Exception  // Placate the compiler.
    }
}

