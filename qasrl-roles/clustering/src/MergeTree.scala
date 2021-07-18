package qasrl.roles.clustering

import jjm.implicits._

import cats.Applicative
import cats.kernel.CommutativeMonoid
import cats.Monad
import cats.Eval
import cats.Foldable
import cats.Reducible
import cats.UnorderedFoldable
import cats.data.Ior
import cats.data.NonEmptyList
import cats.implicits._

import monocle.macros._

import io.circe.generic.JsonCodec

import scala.annotation.tailrec

sealed trait MergeTree[A] {
  def loss: Double
  def leaves: Vector[MergeTree.Leaf[A]]
  def values: Vector[A]
  def valuesWithLosses: Vector[(Double, A)]

  import MergeTree._

  def deltaOpt: Option[Double] = this match {
    case Leaf(_, _) => None
    case Merge(loss, left, right) => Some(loss - left.loss - right.loss)
  }

  def map[B](f: A => B): MergeTree[B] = this match {
    case Leaf(l, a) => Leaf(l, f(a))
    case Merge(l, left, right) => Merge(l, left.map(f), right.map(f))
  }

  // TODO fix interactions with loss
  def flatMap[B](f: A => MergeTree[B]): MergeTree[B] = this match {
    case Leaf(l, a) => f(a)
    case Merge(l, left, right) => Merge(l, left.flatMap(f), right.flatMap(f))
  }

  def cutMap[B](
    shouldKeep: Merge[A] => Boolean,
    f: MergeTree[A] => B
  ): MergeTree[B] = this match {
    case l @ Leaf(loss, _) => Leaf(loss, f(l))
    case m @ Merge(loss, left, right) =>
      if(shouldKeep(m)) {
        Merge(loss, left.cutMap(shouldKeep, f), right.cutMap(shouldKeep, f))
      } else Leaf(loss, f(m))
  }

  // TODO: cut at N?

  // def getMinDeltaForNClusters(n: Int): Option[Double] = {
  //   this.splitToN(scala.math.max(n, 1)).flatMap(_.deltaOpt).minimumOption
  // }

  def cutToN[B](n: Int) = {
    var tree = Leaf(this.loss, this): MergeTree[MergeTree[A]]
    var nextOpt = MergeTree.splitNext(tree)
    while(nextOpt.exists(_.size <= n)) {
      tree = nextOpt.get // ok because of exists check
      nextOpt = MergeTree.splitNext(tree)
    }
    tree
  }

  def cutMapAtN[B](n: Int, f: MergeTree[A] => B) =
    cutToN(n).map(f)

  def cut(shouldKeep: Merge[A] => Boolean): MergeTree[MergeTree[A]] =
    cutMap(shouldKeep, identity)

  def splitWhile(shouldSplit: Merge[A] => Boolean): Vector[MergeTree[A]] =
    cut(shouldSplit).values

  def splitToN(n: Int): Vector[MergeTree[A]] = {
    require(n >= 1)
    // using vars bc too lazy to look into foldRight on stream
    var splits = this.clusterSplittings
    var cur = splits.head
    while(cur.size < n && splits.headOption.nonEmpty) {
      splits = splits.tail
      cur = splits.head
    }
    cur
  }

  def isLeaf: Boolean = this match {
    case Leaf(_, _) => true
    case _ => false
  }
  def isMerge: Boolean = this match {
    case Merge(_, _, _) => true
    case _ => false
  }

  def withMaxDepth(maxDepth: Int)(implicit M: CommutativeMonoid[A]) = hylo[
    (Int, MergeTree[A]), (Int, A), MergeTree[A]](
    destruct = {
      case (curDepth, tree) =>
        if(curDepth >= maxDepth) {
          Left(tree.loss -> (curDepth -> tree.unorderedFold))
        } else project(tree) match {
          case Left((loss, b)) => Left(loss -> (curDepth -> b))
          case Right((loss, l, r)) => Right((loss, (curDepth + 1) -> l, (curDepth + 1) -> r))
        }
    },
    construct = makeAlgebra(
      leaf = { case (loss, (depth, value)) => Leaf(loss, value) },
      merge = { case (loss, left, right) => Merge(loss, left, right) }
    )
  )(0 -> this)

  // def extractPureTrees[B](index: A => B): Map[B, Vector[MergeTree[A]]] = {
  //   extractPureTreesAux(index).left.map { case (idx, tree) => idx -> Vector(tree) }.toMap.merge
  // }
  // // left: this tree is pure. right: this tree is not pure; here are the pure subtrees.
  // def extractPureTreesAux[B](index: A => B): Either[(B, MergeTree[A]), Map[B, Vector[MergeTree[A]]]] = {
  //   cata[Map[B, Vector[MergeTree[A]]]](
  //     leaf = (_, value) => Left(index(value) -> this),
  //     merge = (_, left, right) => left
  //       .left.toOption.map(_._1).flatMap { b =>
  //         right.left.toOption.map(_._1).filter(_ == b).as(
  //           Left(b -> this)
  //         )
  //       }.getOrElse {
  //         Right(
  //           left.left.map { case (idx, tree) => idx -> Vector(tree) }.toMap.merge |+|
  //             right.left.map { case (idx, tree) => idx -> Vector(tree) }.toMap.merge
  //         )
  //       }
  //   )
  // }

  // partition by an indexing function, but keep the overarching tree structure.
  // Each returned tree is a like a "view" on the original which ignores values outside its index,
  // but retains loss values.
  def groupBy[B](index: A => B): Map[B, MergeTree[A]] = {
    cata[Map[B, MergeTree[A]]](
      leaf = (loss, value) => Map(index(value) -> Leaf(loss, value)),
      merge = (loss, left, right) => left.merge(right).transform {
        case (_, treeIor) => treeIor match {
          case Ior.Left(tree) => tree
          case Ior.Right(tree) => tree
          case Ior.Both(l, r) => Merge(loss, l, r)
        }
      }
    )
  }

  // partition by an indexing function, like groupBy, but allows the inside to be grouped as well.
  def group[B](index: A => Map[B, A]): Map[B, MergeTree[A]] = {
    cata[Map[B, MergeTree[A]]](
      leaf = (loss, value) => index(value).mapVals(Leaf(loss, _)),
      merge = (loss, left, right) => left.merge(right).transform {
        case (_, treeIor) => treeIor match {
          case Ior.Left(tree) => tree
          case Ior.Right(tree) => tree
          case Ior.Both(l, r) => Merge(loss, l, r)
        }
      }
    )
  }

  // returns Some if value appears at most once in the merge tree
  def clustersForValue(value: A): Option[List[MergeTree[A]]] = this match {
    case Leaf(_, `value`) => Some(List(this)) // 0 because thresholding doesn't affect leaves
    case Merge(loss, left, right) =>
      left.clustersForValue(value)
        .orElse(right.clustersForValue(value))
        .map(this :: _)
    case _ => None
  }

  def clusterSplittings = MergeTree.clusterSplittings(Vector(this))
  def clusterSplittingsStream = MergeTree.clusterSplittingsStream(this)
  def clusterSplittingsStreamByMaxLossPerItem =
    MergeTree.clusterSplittingsStreamByMaxLossPerItem(this)
  def clusterSplittingsStreamByMaxLossPerCount(count: A => Double) =
    MergeTree.clusterSplittingsStreamByMaxLossPerCount(this, count)

  def cata[B](
    leaf: (Double, A) => B,
    merge: (Double, B, B) => B
  ): B = MergeTree.cata(MergeTree.makeAlgebra(leaf, merge))(this)

  def depth = cata[Int](
    leaf = (_, _) => 0,
    merge = (_, l, r) => scala.math.max(l, r) + 1
  )

  def contains(x: A) = cata[Boolean](
    leaf = (_, y) => x == y,
    merge = (_, y, z) => y || z
  )

  def toJsonStringSafe(implicit e: io.circe.Encoder[A]) = {
    import io.circe.syntax._
    cata[Vector[String]](
      leaf = (loss, value) => Vector(Leaf(loss, value).asJson.noSpaces),
      merge = (loss, left, right) => Vector(
        Vector("{\"loss\": " + loss.asJson.noSpaces + ", \"left\": "),
        left,
        Vector(", \"right\": "),
        right,
        Vector("}")
      ).flatten
    ).mkString
  }
}
object MergeTree {

  def createBalancedTree[A](inputs: NonEmptyList[A]): MergeTree[A] = {
    var nodes: List[MergeTree[A]] = inputs.toList.map(a => Leaf(0.0, a))
    while(nodes.size > 1) {
      nodes = nodes.grouped(2).map(_.toList).flatMap {
        case l :: r :: Nil =>
          List(Merge(0.0, l, r))
        case x => x
      }.toList
    }
    nodes.head
  }

  // def reduceLossThreshold[A](trees: Vector[MergeTree[A]]): Vector[MergeTree[A]] = {
  //   val maxLoss = trees.map(_.loss).max
  //   trees.flatMap(_.splitByPredicate(_.loss >= maxLoss)) -> maxLoss
  // }
  def clusterSplittings[A](trees: Vector[MergeTree[A]]): Stream[Vector[MergeTree[A]]] = {
    trees #:: trees.flatMap(_.deltaOpt).maximumOption.foldMap { maxDelta =>
      val newTrees = trees.flatMap(t =>
        MergeTree.merge.getOption(t)
          .filter(_.delta >= maxDelta)
          .fold(Vector(t))(m => Vector(m.left, m.right))
      )
      clusterSplittings(newTrees)
    }
  }

  def clusterSplittingsStream[A](tree: MergeTree[A]): fs2.Stream[cats.Id, Vector[MergeTree[A]]] = {
    fs2.Stream.iterate(Vector(tree)) { trees =>
      trees.flatMap(_.deltaOpt).maximumOption.foldMap { maxDelta =>
        trees.flatMap(t =>
          MergeTree.merge.getOption(t)
            .filter(_.delta >= maxDelta)
            .fold(Vector(t))(m => Vector(m.left, m.right))
        )
      }
    }.takeWhile(_.nonEmpty)
  }

  def clusterSplittingsStreamByMaxLossPerItem[A](tree: MergeTree[A]): fs2.Stream[cats.Id, Vector[MergeTree[A]]] = {
    fs2.Stream.iterate(Vector(tree)) { trees =>
      trees.filter(_.isMerge).map(t => t.loss / t.size).maximumOption.foldMap { maxLossPerItem =>
        trees.flatMap(t =>
          MergeTree.merge.getOption(t)
            .filter(_ => t.loss / t.size >= maxLossPerItem)
            .fold(Vector(t))(m => Vector(m.left, m.right))
        )
      }
    }.takeWhile(_.nonEmpty)
  }

  def clusterSplittingsStreamByMaxLossPerCount[A](tree: MergeTree[A], count: A => Double): fs2.Stream[cats.Id, Vector[MergeTree[A]]] = {
    def getSize(t: MergeTree[A]) = t.unorderedFoldMap(count)
    fs2.Stream.iterate(Vector(tree)) { trees =>
      trees.filter(_.isMerge).map(t => t.loss / getSize(t)).maximumOption.foldMap { maxLossPerItem =>
        trees.flatMap(t =>
          MergeTree.merge.getOption(t)
            .filter(_ => t.loss / getSize(t) >= maxLossPerItem)
            .fold(Vector(t))(m => Vector(m.left, m.right))
        )
      }
    }.takeWhile(_.nonEmpty)
  }

  def clusterSplittingsStreamByMaxLossDelta[A](tree: MergeTree[A]): fs2.Stream[cats.Id, Vector[MergeTree[A]]] = {
    def getDelta(t: MergeTree[A]) = t match {
      case Leaf(_, _) => 0.0
      case Merge(loss, left, right) => loss - left.loss - right.loss
    }
    // def getSize(t: MergeTree[A]) = t.unorderedFoldMap(count)
    fs2.Stream.iterate(Vector(tree)) { trees =>
      trees.filter(_.isMerge).map(getDelta).maximumOption.foldMap { maxDelta =>
        trees.flatMap(t =>
          MergeTree.merge.getOption(t)
            .filter(_ => getDelta(t) >= maxDelta)
            .fold(Vector(t))(m => Vector(m.left, m.right))
        )
      }
    }.takeWhile(_.nonEmpty)
  }

  def splitNext[A](tree: MergeTree[MergeTree[A]]): Option[MergeTree[MergeTree[A]]] = {
    tree.values.flatMap(_.deltaOpt).maximumOption.map(maxDelta =>
      tree.flatMap(innerTree =>
        MergeTree.merge.getOption(innerTree)
          .filter(_.delta >= maxDelta)
          .fold(Leaf(0.0, innerTree): MergeTree[MergeTree[A]])(m =>
            Merge(m.loss, Leaf(m.left.loss, m.left), Leaf(m.right.loss, m.right))
          )
      )
    )
  }

  type Algebra[B, A] = Either[(Double, B), (Double, A, A)] => A
  type AlgebraM[F[_], B, A] = Either[(Double, B), (Double, A, A)] => F[A]
  def makeAlgebra[B, A](
    leaf: (Double, B) => A,
    merge: (Double, A, A) => A
  ): Algebra[B, A] = {
    (params: Either[(Double, B), (Double, A, A)]) => params match {
      case Left((loss, value)) => leaf(loss, value)
      case Right((loss, left, right)) => merge(loss, left, right)
    }
  }
  def makeAlgebraM[F[_], B, A](
    leaf: (Double, B) => F[A],
    merge: (Double, A, A) => F[A]
  ): AlgebraM[F, B, A] = {
    (params: Either[(Double, B), (Double, A, A)]) => params match {
      case Left((loss, value)) => leaf(loss, value)
      case Right((loss, left, right)) => merge(loss, left, right)
    }
  }

  type Coalgebra[B, A] = A => Either[(Double, B), (Double, A, A)]
  type CoalgebraM[F[_], B, A] = A => F[Either[(Double, B), (Double, A, A)]]

  def embed[A]: Algebra[A, MergeTree[A]] = makeAlgebra[A, MergeTree[A]](
    Leaf(_, _), Merge(_, _, _)
  )
  def embedM[F[_]: Applicative, A]: AlgebraM[F, A, MergeTree[A]] =
    embed[A] andThen Applicative[F].pure[MergeTree[A]]

  def project[A]: Coalgebra[A, MergeTree[A]] = (tree: MergeTree[A]) => tree match {
    case Leaf(loss, value) => Left(loss -> value)
    case Merge(loss, left, right) => Right((loss, left, right))
  }
  def projectM[F[_]: Applicative, A]: CoalgebraM[F, A, MergeTree[A]] = {
    project[A] andThen Applicative[F].pure[Either[(Double, A), (Double, MergeTree[A], MergeTree[A])]]
  }

  import cats.Monad

  // not inherently stack-safe; must be used with a stack-safe monad
  def hyloM[F[_]: Monad, A, B, C](
    destructM: CoalgebraM[F, B, A],
    constructM: AlgebraM[F, B, C]
  ): A => F[C] = (a: A) => {
    def loop( // TODO all below
      toVisit: List[A],
      toCollect: List[Either[Double, F[C]]]
    ): F[C] = toCollect match {
      case Right(rightM) :: Right(leftM) :: Left(loss) :: tail =>
        // eagerly merge to keep the stack size small
        val merged = for {
          left <- leftM
          right <- rightM
          merge <- constructM(Right((loss, left, right)))
        } yield merge
        loop(toVisit, Right(merged) :: tail)
      case _ => toVisit match {
        case a :: next => destructM(a).flatMap {
          case Left((loss, value)) =>
            loop(next, Right(constructM(Left(loss -> value))) :: toCollect)
          case Right((loss, left, right)) =>
            loop(left :: right :: next, Left(loss) :: toCollect)
        }
        case Nil => toCollect.head.right.get // should always work
      }
    }
    loop(List(a), Nil)
  }
  def anaM[F[_]: Monad, A, B](destruct: CoalgebraM[F, B, A]): A => F[MergeTree[B]] = {
    hyloM(destruct, embedM[F, B])
  }
  def cataM[F[_]: Monad, A, B](construct: AlgebraM[F, A, B]): MergeTree[A] => F[B] = {
    hyloM(projectM[F, A], construct)
  }

  // stack-safe backbone for tree transformations
  def hylo[A, B, C](
    destruct: Coalgebra[B, A],
    construct: Algebra[B, C]
  ): A => C = (a: A) => {
    @tailrec
    def loop(
      toVisit: List[A],
      toCollect: List[Either[Double, C]]
    ): C = toCollect match {
      case Right(right) :: Right(left) :: Left(loss) :: tail =>
        // eagerly merge to keep the stack size small
        loop(toVisit, Right(construct(Right((loss, left, right)))) :: tail)
      case _ => toVisit match {
        case a :: next => destruct(a) match {
          case Left((loss, value)) =>
            loop(next, Right(construct(Left(loss -> value))) :: toCollect)
          case Right((loss, left, right)) =>
            loop(left :: right :: next, Left(loss) :: toCollect)
        }
        case Nil => toCollect.head.right.get
      }
    }
    loop(List(a), Nil)
  }
  def ana[A, B](destruct: Coalgebra[B, A]): A => MergeTree[B] = {
    hylo(destruct, embed[B])
  }
  def cata[A, B](construct: Algebra[A, B]): MergeTree[A] => B = {
    hylo(project[A], construct)
  }

  // stack-safe implementation of hylo particular to the Either monad (for json decoding)
  def hyloEither[E, A, B, C](
    destructM: CoalgebraM[Either[E, *], B, A],
    constructM: AlgebraM[Either[E, *], B, C]
  ): A => Either[E, C] = (a: A) => {
    @tailrec
    def loop(
      toVisit: List[A],
      toCollect: List[Either[Double, C]]
    ): Either[E, C] = toCollect match {
      case Right(right) :: Right(left) :: Left(loss) :: tail =>
        // eagerly merge to keep the stack size small
        constructM(Right((loss, left, right))) match {
          case Right(merged) => loop(toVisit, Right(merged) :: tail)
          case Left(error) => Left(error)
        }
      case _ => toVisit match {
        case a :: next => destructM(a) match {
          case Left(error) => Left(error)
          case Right(Left((loss, value))) =>
            constructM(Left(loss -> value)) match {
              case Left(error) => Left(error)
              case Right(leaf) => loop(next, Right(leaf) :: toCollect)
            }
          case Right(Right((loss, left, right))) =>
            loop(left :: right :: next, Left(loss) :: toCollect)
        }
        case Nil => Right(toCollect.head.right.get) // should always work
      }
    }
    loop(List(a), Nil)
  }
  def anaEither[E, A, B](destruct: CoalgebraM[Either[E, *], B, A]): A => Either[E, MergeTree[B]] = {
    hyloEither(destruct, embedM[Either[E, *], B])
  }
  def cataEither[E, A, B](construct: AlgebraM[Either[E, *], A, B]): MergeTree[A] => Either[E, B] = {
    hyloEither(projectM[Either[E, *], A], construct)
  }

  // def tailRecM[B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
  //   @tailrec
  //   def loop(toVisit: List[Tree[Either[A, B]]],
  //            toCollect: List[Option[Tree[B]]]): List[Tree[B]] =
  //     toVisit match {
  //       case Branch(l, r) :: next =>
  //         loop(l :: r :: next, None :: toCollect)

  //       case Leaf(Left(value)) :: next =>
  //         loop(func(value) :: next, toCollect)

  //       case Leaf(Right(value)) :: next =>
  //         loop(next, Some(pure(value)) :: toCollect)

  //       case Nil =>
  //         toCollect.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
  //           maybeTree.map(_ :: acc).getOrElse {
  //             val left :: right :: tail = acc
  //             branch(left, right) :: tail
  //           }
  //         }
  //     }

  //   loop(List(func(arg)), Nil).head
  // }

  @Lenses @JsonCodec case class Leaf[A](
    loss: Double, value: A) extends MergeTree[A] {
    def leaves: Vector[MergeTree.Leaf[A]] = Vector(this)
    def values: Vector[A] = Vector(value)
    def valuesWithLosses: Vector[(Double, A)] = Vector(loss -> value)
  }
  object Leaf
  @Lenses case class Merge[A](
    loss: Double,
    left: MergeTree[A],
    right: MergeTree[A]
  ) extends MergeTree[A] {
    def leaves: Vector[MergeTree.Leaf[A]] = left.leaves ++ right.leaves
    def values: Vector[A] = left.values ++ right.values
    def valuesWithLosses: Vector[(Double, A)] = left.valuesWithLosses ++ right.valuesWithLosses
    def delta: Double = loss - left.loss - right.loss
  }
  object Merge

  import io.circe._
  import io.circe.syntax._

  implicit def mergeTreeEncoder[A: Encoder]: Encoder[MergeTree[A]] =
    Encoder.instance[MergeTree[A]](
      _.cata[Json](
        leaf = (loss, value) => Json.obj(
          "loss" -> loss.asJson, "value" -> value.asJson
        ),
        merge = (loss, left, right) => Json.obj(
          "loss" -> loss.asJson, "left" -> left, "right" -> right
        )
      ))
  implicit def mergeTreeDecoder[A: Decoder]: Decoder[MergeTree[A]] = {
    Decoder.instance[MergeTree[A]](
      anaEither[DecodingFailure, ACursor, A](c =>
        if(c.downField("value").succeeded) {
          for {
            loss <- c.downField("loss").as[Double]
            value <- c.downField("value").as[A]
          } yield Left((loss, value))
        } else {
          for {
            loss <- c.downField("loss").as[Double]
          } yield Right((loss, c.downField("left"), c.downField("right")))
        }
      )
    )
  }

  def leaf[A] = GenPrism[MergeTree[A], Leaf[A]]
  def merge[A] = GenPrism[MergeTree[A], Merge[A]]

  implicit val mergeTreeFoldable: UnorderedFoldable[MergeTree] = {
    new UnorderedFoldable[MergeTree] {
      type F[A] = MergeTree[A]

      // def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
      //   fa match {
      //     case Leaf(_, value) => f(value)
      //     case Merge(_, left, right) =>
      //       foldLeft(right, reduceLeftTo(left)(f)(g))(g)
      //   }
      // }

      // // TODO make this properly lazy or... ?? not sure if this works exactly as I expect
      // def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
      //   fa match {
      //     case Leaf(_, value) => Eval.later(f(value))
      //     case Merge(_, left, right) =>
      //       foldRight(left, reduceRightTo(right)(f)(g))(g)
      //   }
      // }

      def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B = fa.cata[B](
        leaf = (_, value) => f(value),
        merge = (_, left, right) => left |+| right
      )

      // def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa match {
      //   case Leaf(_, value) => f(b, value)
      //   case Merge(_, left, right) =>
      //     foldLeft(right, foldLeft(left, b)(f))(f)
      // }

      // def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      //   fa match {
      //     case Leaf(_, value) => f(value, lb)
      //     case Merge(_, left, right) =>
      //       foldRight(left, foldRight(right, lb)(f))(f)
      //   }
      // }
    }
  }
}

