package qasrl.roles.clustering

import cats.Order
import cats.implicits._

import scala.annotation.tailrec

class SplayTreeQueue[A: Order](
  var root: Option[SplayTreeNode[A]]
) {
  def popMin(): Option[(A, Int)] = root.map { node =>
    val (min, n) = node.min
    min.splay()

    require(min.left.isEmpty)
    root = min.right
    root.foreach(_.parent = None)
    min.value -> n
  }
  def insert(a: A): Int = root match {
    case None =>
      root = Some(new SplayTreeNode(a, None, None, None))
      0
    case Some(r) =>
      val (newNode, n) = r.insert(a)
      newNode.splay()
      root = Some(newNode)
      n
  }
}

class SplayTreeNode[A](
  val value: A,
  var left: Option[SplayTreeNode[A]],
  var right: Option[SplayTreeNode[A]],
  var parent: Option[SplayTreeNode[A]]
) {
  def pretty: String = pretty(1)
  def pretty(indentLevel: Int): String = {
    val indent = "  " * indentLevel
    val leftStr = left.fold("")(_.pretty(indentLevel + 1))
    val rightStr = right.fold("")(_.pretty(indentLevel + 1))
    s"$value\n${indent}left: $leftStr\n${indent}right: $rightStr"
  }

  def isSorted(implicit o: Order[A]) = isSortedWithInterval.nonEmpty

  def isSortedWithInterval(implicit o: Order[A]): Option[(A, A)] = for {
    min <- left.map(
      _.isSortedWithInterval.flatMap { case (lo, hi) =>
        if(hi <= this.value) Some(lo) else None
      }
    ).getOrElse(Some(this.value))
    max <- right.map(
      _.isSortedWithInterval.flatMap { case (lo, hi) =>
        if(lo >= this.value) Some(hi) else None
      }
    ).getOrElse(Some(this.value))
  } yield (min, max)

  def isRoot = parent.isEmpty

  def size: Int = 1 + left.fold(0)(_.size) + right.fold(0)(_.size)

  def min: (SplayTreeNode[A], Int) = minAux(1)

  @tailrec private def minAux(n: Int): (SplayTreeNode[A], Int) = left match {
    case None => this -> n
    case Some(x) => x.minAux(n + 1)
  }

  def insert(a: A)(implicit o: Order[A]): (SplayTreeNode[A], Int) = insertAux(a, 1)
  @tailrec private def insertAux(a: A, n: Int)(implicit o: Order[A]): (SplayTreeNode[A], Int) = {
    if(a <= this.value) left match {
      case None =>
        val x = new SplayTreeNode(a, None, None, Some(this))
        this.left = Some(x)
        x -> n
      case Some(l) => l.insertAux(a, n + 1)
    } else right match {
      case None =>
        val x = new SplayTreeNode(a, None, None, Some(this))
        this.right = Some(x)
        x -> n
      case Some(r) => r.insertAux(a, n + 1)
    }
  }

  private def isLeftChildOf(x: SplayTreeNode[A]) =
    x.left.exists(_ == this)
  private def isRightChildOf(x: SplayTreeNode[A]) =
    x.right.exists(_ == this)
  private def setLeftChild(x: Option[SplayTreeNode[A]]) = {
    this.left = x
    x.foreach(_.parent = Some(this))
  }
  private def setRightChild(x: Option[SplayTreeNode[A]]) = {
    this.right = x
    x.foreach(_.parent = Some(this))
  }

  // TODO this probably would have just been better if written in terms of rotations...
  @tailrec final def splay(): Unit = parent match {
    case None => ()
    case Some(p) =>
      p.parent match {
        case None =>
          if(this.isLeftChildOf(p)) {
            // left zig
            p.setLeftChild(this.right)
            this.setRightChild(Some(p))
          } else {
            require(this.isRightChildOf(p))
            // right zig
            p.setRightChild(this.left)
            this.setLeftChild(Some(p))
          }
          this.parent = None
        case Some(g) =>
          g.parent match {
            case None => this.parent = None
            case Some(gg) =>
              if(g.isLeftChildOf(gg)) gg.setLeftChild(Some(this))
              else {
                require(g.isRightChildOf(gg))
                gg.setRightChild(Some(this))
              }
          }
          if(p.isLeftChildOf(g) && this.isLeftChildOf(p)) {
            // left zig-zig
            p.setLeftChild(this.right)
            g.setLeftChild(p.right)
            p.setRightChild(Some(g))
            this.setRightChild(Some(p))
          } else if(p.isRightChildOf(g) && this.isRightChildOf(p)) {
            // right zig-zig
            p.setRightChild(this.left)
            g.setRightChild(p.left)
            p.setLeftChild(Some(g))
            this.setLeftChild(Some(p))
          } else if(p.isLeftChildOf(g) && this.isRightChildOf(p)) {
            // left-right zig-zag
            p.setRightChild(this.left)
            g.setLeftChild(this.right)
            this.setLeftChild(Some(p))
            this.setRightChild(Some(g))
          } else {
            require(p.isRightChildOf(g) && this.isLeftChildOf(p))
            // right-left zig-zag
            p.setLeftChild(this.right)
            g.setRightChild(this.left)
            this.setRightChild(Some(p))
            this.setLeftChild(Some(g))
          }
          this.splay()
      }
  }
}

object SplayTreeQueue {
  def nodeFromSortedVector[A](parent: Option[SplayTreeNode[A]], vec: Vector[A]): Option[SplayTreeNode[A]] = {
    if(vec.isEmpty) None else Some {
      val (left, rootAndRight) = vec.splitAt(vec.size / 2)
      val value = rootAndRight.head
      val right = rootAndRight.tail
      val n = new SplayTreeNode(value, None, None, parent)
      n.left = nodeFromSortedVector(Some(n), left)
      n.right = nodeFromSortedVector(Some(n), right)
      n
    }
  }
  def fromSortedVector[A: Order](vec: Vector[A]): SplayTreeQueue[A] = {
    val res = new SplayTreeQueue(nodeFromSortedVector(None, vec))
    require(res.root.fold(0)(_.size) == vec.size)
    require(res.root.forall(_.isSorted))
    res
  }
}
