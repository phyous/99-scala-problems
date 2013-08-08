// P91 (**) Knight's tour.
//     Another famous problem is this one: How can a knight jump on an N×N
//     chessboard in such a way that it visits every square exactly once?
//
//     Hints: Represent the squares by pairs of their coordinates of the form
//     (X, Y), where both X and Y are integers between 1 and N. (Alternately,
//     define a Point class for the same purpose.)  Write a function
//     jump(N, (X, Y), (U, V)) to express the fact that a knight can jump from
//     (X, Y) to (U, V) on a N×N chessboard.  And finally, represent the
//     solution of our problem as a list of knight positions (the knight's
//     tour).

case class Point(x: Int, y: Int) {
  def +(o: Point) = Point(x + o.x, y + o.y)
  def -(o: Point) = Point(x - o.x, y - o.y)
  def +(t: (Int,Int)) = Point(x + t._1, y + t._2)
  def -(t: (Int,Int)) = Point(x - t._1, y - t._2)
  def jumps(n: Int): List[Point] = 
    List((2, 1), (1, 2), (-1, 2), (-2, 1)) flatMap {x =>
      List(this + x, this - x)
    } filter {p =>
      (p.x min p.y) >= 1 && (p.x max p.y) <= n
    }
}

object P91 {
  // Utility function.
  // Filters out the positions already seen in the given set, and orders the
  // points with the one with the fewest possibilities first, in line with
  // Warnsdorff's heuristic.
  def jumps(p: Point, n: Int, s: Set[Point]): List[Point] = {
    def filtered(p: Point) = p.jumps(n).remove(s(_))
    filtered(p).sort(filtered(_).length < filtered(_).length)
  }

  // Find one tour.
  // Because we're using the stack for our state here, one of the simplest ways
  // to signal a result is to throw an exception and not worry about properly
  // unwinding the stack.
  class TourFound(val tour: List[Point]) extends Exception
  def knightsTour(n: Int): List[Point] = knightsTour(n, Point(1, 1), (p,s) => s.size == n*n)
  def knightsTourClosed(n: Int): List[Point] =
    knightsTour(n, Point(1, 1), (p,s) => s.size == n*n && p.jumps(n).contains(Point(1, 1)))
  def knightsTour(n: Int, start: Point, done: (Point,Set[Point]) => Boolean): List[Point] =
    try {
      findPath(n, start, Set(start), List(start), done)
      Nil
    } catch {
      case t: TourFound => t.tour
    }
  def findPath(n: Int, p: Point, s: Set[Point], soFar: List[Point], done: (Point,Set[Point]) => Boolean): Unit =
    if (done(p, s)) throw new TourFound(soFar)
    else jumps(p, n, s).foreach(q => findPath(n, q, s + q, q :: soFar, done))

  // Find all tours.
  // This is actually easier than finding just one.  The drawback, of course,
  // is that there are so many that this will take a long time and use a large
  // amount of memory building the list.
  def knightsTourComplete(n: Int): List[List[Point]] = knightsTourComplete(n, Point(1, 1))
  def knightsTourCompleteClosed(n: Int): List[List[Point]] =
    knightsTourComplete(n, Point(1, 1)).filter(_.head.jumps(n).contains(Point(1, 1)))
  def knightsTourComplete(n: Int, start: Point): List[List[Point]] =
    findAllPaths(n, start, Set(start), List(start))
  def findAllPaths(n: Int, p: Point, s: Set[Point], soFar: List[Point]): List[List[Point]] =
    if (s.size == n*n) List(soFar)
    else jumps(p, n, s).flatMap(q => findAllPaths(n, q, s + q, q :: soFar))

  // Find all tours, lazily.
  // Instead of implicitly embedding our state into the stack, here we
  // explicitly keep a list containing what were stack frames in the other
  // versions.  Also, rather than mapping across a list in each function call,
  // we just make a frame for each potential destination and drop each one onto
  // our imitation stack.
  case class Frame(n: Int, p: Point, s: Set[Point], soFar: List[Point])
  def knightsTourLazy(n: Int): Stream[List[Point]] = knightsTourLazy(n, Point(1, 1))
  def knightsTourLazyClosed(n: Int): Stream[List[Point]] =
    knightsTourLazy(n, Point(1, 1)).filter(_.head.jumps(n).contains(Point(1, 1)))
  def knightsTourLazy(n: Int, start: Point): Stream[List[Point]] =
    nextTour(List(Frame(n, start, Set(start), List(start))))
  def nextTour(stack: List[Frame]): Stream[List[Point]] = stack match {
    case Nil => Stream.empty
    case Frame(n, p, s, soFar) :: tail =>
      if (s.size == n*n) Stream.cons(soFar, nextTour(tail))
      else nextTour(jumps(p, n, s).map(q => Frame(n, q, s + q, q :: soFar)) ::: tail)
  }
}
