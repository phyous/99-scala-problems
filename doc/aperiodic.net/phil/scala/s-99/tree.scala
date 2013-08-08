package binarytree {

  sealed abstract class Tree[+T] {
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
    def nodeCount: Int
    def leafCount: Int = leafList.length
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(level: Int): List[T]
    def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
    def treeDepth: Int
    def leftmostNodeDepth: Int
    def layoutBinaryTree2: Tree[T] = {
      val d = treeDepth
      val x0 = (2 to leftmostNodeDepth).map((n) => Math.pow(2, d - n).toInt).reduceLeft(_+_) + 1
      layoutBinaryTree2Internal(x0, 1, d - 2)
    }
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]
    def bounds: List[(Int,Int)]
    def layoutBinaryTree3: Tree[T] = 
      layoutBinaryTree3Internal(bounds.map(_._1).reduceLeft(_ min _) * -1 + 1, 1)
    def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T]
    def preorder: List[T]
    def inorder: List[T]
    def toDotstring: String
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = (left, right) match {
      case (End, End) => value.toString
      case _ => value.toString + "(" + left + "," + right + ")"
    }
    def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
      case _          => false
    }
    def isSymmetric: Boolean = left.isMirrorOf(right)
    def addValue[U >: T <% Ordered[U]](x: U) =
      if (x < value) Node(value, left.addValue(x), right)
      else Node(value, left, right.addValue(x))
    def nodeCount: Int = left.nodeCount + right.nodeCount + 1
    def leafList: List[T] = (left, right) match {
      case (End, End) => List(value)
      case _          => left.leafList ::: right.leafList
    }
    def internalList: List[T] = (left, right) match {
      case (End, End) => Nil
      case _          => value :: left.internalList ::: right.internalList
    }
    def atLevel(level: Int): List[T] = level match {
      case n if n < 1 => Nil
      case 1          => List(value)
      case n          => left.atLevel(n - 1) ::: right.atLevel(n - 1)
    }
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
      val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
      val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
      (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
    }
    def treeDepth: Int = (left.treeDepth max right.treeDepth) + 1
    def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T] =
      PositionedNode(
        value,
        left.layoutBinaryTree2Internal(x - Math.pow(2, exp).toInt, depth + 1, exp - 1),
        right.layoutBinaryTree2Internal(x + Math.pow(2, exp).toInt, depth + 1, exp - 1),
        x, depth)
    def bounds: List[(Int,Int)] = {
      def lowerBounds = (left.bounds, right.bounds) match {
        case (Nil, Nil) => Nil
        case (lb, Nil)  => lb.map((b) => (b._1 - 1, b._2 - 1))
        case (Nil, rb)  => rb.map((b) => (b._1 + 1, b._2 + 1))
        case (lb, rb) => {
          val shift = lb.zip(rb).map((e) => (e._1._2 - e._2._1) / 2 + 1).reduceLeft(_ max _)
          lb.map(Some(_)).zipAll(rb.map(Some(_)), None, None).map(_ match {
            case (Some((a, b)), Some((c, d))) => (a - shift, d + shift)
            case (Some((a, b)), None)         => (a - shift, b - shift)
            case (None, Some((c, d)))         => (c + shift, d + shift)
            case (None, None) => throw new Exception  // Placate the compiler; can't get here.
          })
        }
      }
      (0, 0) :: lowerBounds
    }
    def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T] = bounds match {
      case _ :: (bl, br) :: _ => PositionedNode(
        value, left.layoutBinaryTree3Internal(x + bl, depth + 1),
        right.layoutBinaryTree3Internal(x + br, depth + 1), x, depth)
      case _ => PositionedNode(value, End, End, x, depth)
    }
    def preorder: List[T] = value :: left.preorder ::: right.preorder
    def inorder: List[T] = left.inorder ::: value :: right.inorder
    def toDotstring: String = value.toString + left.toDotstring + right.toDotstring
  }

  case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = (left, right) match {
      case (End, End) => value + "[" + x + "," + y + "]"
      case _ => value + "[" + x + "," + y + "](" + left + "," + right + ")"
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = ""
    def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
    def isSymmetric: Boolean = true
    def addValue[U <% Ordered[U]](x: U) = Node(x)
    def nodeCount: Int = 0
    def leafList = Nil
    def internalList = Nil
    def atLevel(level: Int) = Nil
    def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)
    def treeDepth: Int = 0
    def leftmostNodeDepth: Int = 0
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int) = End
    def bounds: List[(Int,Int)] = Nil
    def layoutBinaryTree3Internal(x: Int, depth: Int) = End
    def preorder = Nil
    def inorder = Nil
    def toDotstring: String = "."
  }

  object Tree {
    def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if n < 1 => List(End)
      case n if n % 2 == 1 => {
        val subtrees = cBalanced(n / 2, value)
        subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
      }
      case n if n % 2 == 0 => {
        val lesserSubtrees = cBalanced((n - 1) / 2, value)
        val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
        lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
      }
    }
    def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = 
      l.foldLeft(End: Tree[T])((r, e) => r.addValue(e))
    def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] =
      cBalanced(nodes, value).filter(_.isSymmetric)
    def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
      case n if n < 1 => List(End)
      case 1          => List(Node(value))
      case _ => {
        val fullHeight = hbalTrees(height - 1, value)
        val short = hbalTrees(height - 2, value)
        fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
        fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
      }
    }
    def minHbalNodes(height: Int): Int = height match {
      case n if n < 1 => 0
      case 1          => 1
      case n          => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
    }
    def maxHbalNodes(height: Int): Int = 2 * height - 1
    def minHbalHeight(nodes: Int): Int =
      if (nodes == 0) 0
      else minHbalHeight(nodes / 2) + 1
    def maxHbalHeight(nodes: Int): Int = 
      Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last
    def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
      (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList
    def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
      def generateTree(addr: Int): Tree[T] =
        if (addr > nodes) End
        else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
      generateTree(1)
    }
    def string2Tree(s: java.lang.String): Tree[Char] = {
      def extractTreeString(s: String, start: Int, end: Char): (String,Int) = {
        def updateNesting(nesting: Int, pos: Int): Int = s(pos) match {
          case '(' => nesting + 1
          case ')' => nesting - 1
          case _   => nesting
        }
        def findStringEnd(pos: Int, nesting: Int): Int = 
          if (s(pos) == end && nesting == 0) pos
          else findStringEnd(pos + 1, updateNesting(nesting, pos))
        val strEnd = findStringEnd(start, 0)
        (s.substring(start, strEnd), strEnd)
      }
      s.length match {
        case 0 => End
        case 1 => Node(s(0))
        case _ => {
          val (leftStr, commaPos) = extractTreeString(s, 2, ',')
          val (rightStr, _) = extractTreeString(s, commaPos + 1, ')')
          Node(s(0), string2Tree(leftStr), string2Tree(rightStr))
        }
      }
    }
    def preInTree[T](pre: List[T], in: List[T]): Tree[T] = pre match {
      case Nil       => End
      case v :: preTail => {
        val (leftIn, rightIn) = in.span(_ != v)
        Node(v, preInTree(preTail.take(leftIn.length), leftIn),
             preInTree(preTail.drop(leftIn.length), rightIn))
      }
    }
    def fromDotstring(ds: String): Tree[Char] = {
      def fromDotstringR(pos: Int): (Tree[Char], Int) = ds(pos) match {
        case '.' => (End, pos + 1)
        case c   => {
          val (lTree, lPos) = fromDotstringR(pos + 1)
          val (rTree, rPos) = fromDotstringR(lPos)
          (Node(c, lTree, rTree), rPos)
        }
      }
      fromDotstringR(0)._1
    }
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }
}
