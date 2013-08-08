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

// Sudoku can be viewed as an exact cover problem.  One very efficient
// algorithm for solving exact cover problems is Donald Knuth's Algorithm DLX
// as described in his paper "Dancing Links".
//
// Note that the n-queens problem (P90) is also an exact cover problem; solving
// it with Algorithm DLX is left as an exercise for the reader.

// This first section is a generic implementation of Algorithm DLX.  It is,
// obviously, very mutable.
//
// To use this in the most direct way, instantiate a DancingRoot of the
// appropriate type, then call its `add` method to add each of the column
// headers.  Following that, for each row, call <header>.add on the leftmost
// column in the row and then <header>.add(<last node>) after that.
//
// There are convenience functions in the DancingLinks object to create a root
// with a list of headers in one go, and to add rows to an existing matrix.
// `addRows` is O(n) in the number of headers.  If you have a horizontally-
// sparse matrix, you'll probably want to put the headers in an Array and
// refer to them explicitly to add rows.

sealed trait DancingLink[T] {
  type RowClass <: DancingLink[T]
  var up: DancingLink[T]   = this
  var down: DancingLink[T] = this
  var left: RowClass
  var right: RowClass
  var header: DancingHeader[T]

  def dirNodes(next: (DancingLink[T]) => DancingLink[T]): List[DancingLink[T]] = {
    def dirNodesR(start: DancingLink[T], cur: DancingLink[T], result: List[DancingLink[T]]): List[DancingLink[T]] =
      if (cur.eq(start)) result.reverse
      else dirNodesR(start, next(cur), cur :: result)
    dirNodesR(this, next(this), Nil)
  }
  def upNodes: List[DancingLink[T]] = dirNodes(_.up)
  def downNodes: List[DancingLink[T]] = dirNodes(_.down)
  def leftNodes: List[DancingLink[T]] = dirNodes(_.left)
  def rightNodes: List[DancingLink[T]] = dirNodes(_.right)

  def coverVertical: Unit = {
    down.up = up
    up.down = down
    header.size = header.size - 1
  }
  def uncoverVertical: Unit = {
    down.up = this
    up.down = this
    header.size = header.size + 1
  }

  def rowID: Set[T] = rightNodes.foldLeft(Set(header.name))(_ + _.header.name)
}

case class DancingObject[T](var header: DancingHeader[T]) extends DancingLink[T] {
  type RowClass = DancingObject[T]
  var left = this
  var right = this

  def addOnRight(no: DancingObject[T]): DancingObject[T] = {
    no.right = right
    no.left = this
    right.left = no
    right = no
    no
  }
}

sealed trait DancingColumn[T] extends DancingLink[T] {
  type RowClass = DancingColumn[T]
  var left = this
  var right = this
}

case class DancingHeader[T](var name: T) extends DancingColumn[T] {
  var header: DancingHeader[T] = this
  var size = 0

  override def toString = "DancingHeader <" + name.toString + "> (" + size + ")"
  def add: DancingObject[T] = {
    val no = DancingObject(this)
    no.up = up
    no.down = this
    up.down = no
    up = no
    size = size + 1
    no
  }
  def add(leftNode: DancingObject[T]): DancingObject[T] = {
    val no = add
    leftNode.addOnRight(no)
    no
  }

  private def coverHorizontal: Unit = {
    left.right = right
    right.left = left
  }
  private def uncoverHorizontal: Unit = {
    left.right = this
    right.left = this
  }

  def cover: Unit = {
    coverHorizontal
    downNodes.foreach(_.rightNodes.foreach(_.coverVertical))
  }
  def uncover: Unit = {
    upNodes.foreach(_.leftNodes.foreach(_.uncoverVertical))
    uncoverHorizontal
  }
}

class DancingRoot[T] extends DancingColumn[T] {
  var header: DancingHeader[T] = _

  override def toString = "DancingRoot" + (if (isEmpty) " (empty)" else "")

  def isEmpty = right.eq(this)

  def add(name: T): DancingHeader[T] = {
    val nh = DancingHeader(name)
    nh.left = left
    nh.right = this
    left.right = nh
    left = nh
    nh
  }

  def findMinCol: DancingHeader[T] = {
    def findMinColR(hs: List[DancingLink[T]], best: DancingHeader[T]): DancingHeader[T] = 
      hs match {
        case Nil => best
        case (h: DancingHeader[_]) :: tail if h.size == 0 => h
        case (h: DancingHeader[T]) :: tail if h.size < best.size =>
          findMinColR(tail, h)
        case _ :: tail => findMinColR(tail, best)
      }
    right match {
      case h: DancingHeader[_] => findMinColR(rightNodes, h)
      case _ => throw new Exception  // Should never get here.
    }
  }

  def solve: Option[List[Set[T]]] = {
    def solveR(solution: List[Set[T]]): Option[List[Set[T]]] =
      if (isEmpty) Some(solution)
      else {
        val c = findMinCol
        c.cover
        val foundSol = c.downNodes.toStream.map {r =>
          r.rightNodes.foreach(_.header.cover)
          val innerFoundSol = solveR(r.rowID :: solution)
          r.leftNodes.foreach(_.header.uncover)
          innerFoundSol
        }.find(_.isDefined)
        c.uncover
        foundSol match {
          case None => None
          case Some(a) => a
        }
      }
    solveR(Nil)
  }
}

object DancingLinks {
  def mkHeaders[T](hs: Seq[T]): DancingRoot[T] = {
    val root = new DancingRoot[T]
    hs.foreach(root.add(_))
    root
  }
  def addRow[T](root: DancingRoot[T], row: Seq[Boolean]): Unit =
    row.toList.zip(root.rightNodes.toList).foldLeft(List[DancingObject[T]]()) {
      case (r, (e, h: DancingHeader[_])) if e => h.add :: r
      case (r, _) => r
    }.reverse.reduceLeft {(a, b) => a.addOnRight(b); b}
  def addRows[T](root: DancingRoot[T], rows: Seq[Seq[Boolean]]): Unit =
    rows.foreach(addRow(root, _))
}


// Now we need some helper classes for our solution.

// The subclasses of SudokuConstraint make the headers of the DLX matrix.
sealed abstract class SudokuConstraint[T]
case class SudokuCell[T](row: Int, col: Int) extends SudokuConstraint[T]
case class SudokuRow[T](row: Int, value: T) extends SudokuConstraint[T]
case class SudokuCol[T](col: Int, value: T) extends SudokuConstraint[T]
case class SudokuSquare[T](square: Int, value: T) extends SudokuConstraint[T]

// The HeaderContainer classes manage the creation of and access to the headers.
// They give O(1) access, which is important when you have 324 headers.
class SudokuCellHeaderContainer[T](symbols: Set[T], root: DancingRoot[SudokuConstraint[T]]) {
  private val headers = (0 until symbols.size).toList.flatMap(r => (0 until symbols.size).toList.map(c => root.add(SudokuCell(r, c)))).toArray

  def getHeader(row: Int, col: Int): DancingHeader[SudokuConstraint[T]] =
    headers(row * symbols.size + col)
}

class SudokuValHeaderContainer[T](symbols: Set[T], root: DancingRoot[SudokuConstraint[T]], mkConstraint: (Int,T) => SudokuConstraint[T]) {
  private val mapping = Map(symbols.toList.zipWithIndex: _*)
  private val headers = (0 until symbols.size).toList.flatMap(r => symbols.toList.map(v => root.add(mkConstraint(r, v)))).toArray

  def getHeader(symbol: T, group: Int): DancingHeader[SudokuConstraint[T]] =
    headers(group * symbols.size + mapping(symbol))
}


// SudokuSolver does the work of setting up and executing Algorithm DLX.
// It's intentionally very flexible, though it only handles rectangular sudoku.
// squareMapping gives the relation between a cell and the square to which it
// belongs.  This allows for, e.g. six-symbol sudoku where each "square" is a
// 2x3 rectangle in a 6x6 square board.  initialState has an entry for each
// cell that's set in the initial board state.  Empty cells are not present in
// the map.
class SudokuSolver[T](symbols: Set[T], squareMapping: Map[(Int,Int),Int], initialState: Map[(Int,Int),T]) {
  private val root = new DancingRoot[SudokuConstraint[T]]
  private val cells = new SudokuCellHeaderContainer(symbols, root)
  private val rows = new SudokuValHeaderContainer(symbols, root, (r, v: T) => SudokuRow(r, v))
  private val cols = new SudokuValHeaderContainer(symbols, root, (r, v: T) => SudokuCol(r, v))
  private val squares = new SudokuValHeaderContainer(symbols, root, (r, v: T) => SudokuSquare(r, v))
  
  (0 until symbols.size).foreach(r => (0 until symbols.size).foreach {c => 
    if (initialState.contains((r, c))) addValue(initialState((r, c)), r, c)
    else addSet(r, c)
  })
  
  private def addValue(symbol: T, row: Int, col: Int): Unit =
    squares.getHeader(symbol, squareMapping((row, col))).add(
      cols.getHeader(symbol, col).add(
        rows.getHeader(symbol, row).add(
          cells.getHeader(row, col).add)))
  private def addSet(row: Int, col: Int): Unit =
    symbols.foreach(addValue(_, row, col))

  private def extractCellInfo(s: Set[SudokuConstraint[T]]): (Int, Int, T) = 
    s.foldLeft((0, 0, symbols.toList.head)) {
      case ((_, _, v), SudokuCell(nr, nc)) => (nr, nc, v)
      case ((r, c, _), SudokuRow(_, nv))   => (r, c, nv)
      case (a, _) => a
    }

  // If there's no solution, we return None.  If there is a solution, we
  // detangle it from the DLX solution format and return a two-dimensional
  // array.
  def solve: Option[Array[Array[T]]] = {
    val result = new Array[Array[T]](symbols.size, symbols.size)
    root.solve match {
      case None => None
      case Some(a) => {
        a.map(extractCellInfo).foreach {
          case (r, c, v) => result(r)(c) = v
        }
        Some(result)
      }
    }
  }
}

// Convenience object.  Has predefined square mappings for 6- and 9-element
// sudoku, as well as functions to parse, solve, and print 9-element sudoku from
// their customary 81-character string format.
object SudokuSolver {
  val sudoku9Squares = Map((0 to 8).flatMap(r => (0 to 8).map(c => ((r, c), r / 3 * 3 + c / 3))): _*)
  val sudoku6Squares = Map((0 to 5).flatMap(r => (0 to 5).map(c => ((r, c), r / 2 * 2 + c / 3))): _*)
  def solve9String(s: String): Option[Array[Array[Int]]] =
    (new SudokuSolver(Set(1 to 9: _*), sudoku9Squares,
                      Map(s.toList.zipWithIndex.map {
                        case (c, i) => {
                          val n = c - '0'
                          if (1 <= n && n <= 9) Some((i / 9, i % 9) -> n)
                          else None
                        }
                      }.filter(_.isDefined).map(_.get): _*))).solve
  def print9Solution[T](sol: Option[Array[Array[T]]]): Unit = sol match {
    case None => println("No solution.")
    case Some(a) => {
      println(
        (0 until 9 by 3).map {rg =>
          (rg to rg + 2).map {r =>
            (0 until 9 by 3).map {cg =>
              (cg to cg + 2).map {c =>
                a(r)(c).toString
              }.mkString("  ")
            }.mkString(" | ")
          }.mkString("\n")
        }.mkString("\n--------+---------+--------\n"))
    }
  }
}
