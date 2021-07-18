package qasrl.roles.clustering

import jjm.TarjanUnionFind

import cats.Foldable
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector

import cats.effect.IO
import freelog.EphemeralTreeLogger

trait AgglomerativeClusteringAlgorithm {

  private implicit val logLevel = freelog.LogLevel.Debug

  type ClusterParam
  type Index

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam

  // gets loss for an instance wrt a cluster.
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double

  // override eg for max
  def aggregateLosses(
    losses: Vector[Double]
  ): Double = losses.sum

  // override eg for max
  def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    leftParam: ClusterParam,
    rightLoss: Double,
    rightParam: ClusterParam
  ) = newLoss - leftLoss - rightLoss

  // override for efficiency
  // TODO currently assumes nonempty argument
  def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    // indices.iterator.map { index =>
    //   val param = getSingleInstanceParameter(index)
    //   val loss = getInstanceLoss(index, param)
    //   val tree: MergeTree[Index] = MergeTree.Leaf(loss, index)
    //   tree -> param
    // }.reduce { (left, right) =>
    //   val param = mergeParams(left._1, left._2, right._1, right._2)
    //   val loss = mergeLoss(left._1, left._2, right._1, right._2)
    //   val tree: MergeTree[Index] = MergeTree.Merge(loss, left._1, right._1)
    //   tree -> param
    // }._2

    // TODO put this here instead...maybe?
    NonEmptyVector.fromVector(indices).map { innerIndices =>
      runFullAgglomerativeClustering(
        innerIndices
      )(EphemeralTreeLogger.noop[IO, String])
    }.get._2
  }

  def mergeParamsEfficient: Option[(ClusterParam, ClusterParam) => ClusterParam] = None

  // TODO formalize this better so I don't need this.
  def mergeParamsFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = ???

  def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = mergeParamsEfficient match {
    case Some(merge) => merge(leftParam, rightParam)
    case None => mergeParamsFallback(left, leftParam, right, rightParam)
  }

  def mergeLossEfficient: Option[(ClusterParam, ClusterParam) => Double] = None

  def mergeLossFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    val indices = left.values ++ right.values
    val param = mergeParams(left, leftParam, right, rightParam)
    aggregateLosses(indices.map(getInstanceLoss(_, param)))
  }

  def mergeLoss(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = mergeLossEfficient match {
    case Some(merge) => merge(leftParam, rightParam)
    case None => mergeLossFallback(left, leftParam, right, rightParam)
  }

  def runPartialAgglomerativeClustering(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean)(
    implicit Log: EphemeralTreeLogger[cats.effect.IO, String]
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    runPartialAgglomerativeClustering_ManyQueues(initTrees, stoppingCondition)
  }

  // more efficient implementation! possibly MUCH more efficient for good distance functions
  // Also potentially more efficient than the version below, by using a splay tree.
  // BUT: requires monotonicity property on the loss function. This apparently doesn't hold
  // for a bunch of the ones I care about! Lame!!!!
  def runPartialAgglomerativeClustering_Pruned(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean)(
    implicit Log: EphemeralTreeLogger[cats.effect.IO, String]
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    val indices = initTrees.toVector.indices
    val initNumTrees = initTrees.size.toInt

    Log.trace(s"Clustering ${initNumTrees} items.").unsafeRunSync()

    case class MergeCandidate(
      i: Int, j: Int, delta: Double, loss: Double
    )
    object MergeCandidate {
      implicit val mergeCandidateOrdering = Ordering.by[MergeCandidate, Double](_.delta)
      implicit val mergeCandidateOrder = Order.by[MergeCandidate, Double](_.delta)
    }

    // Variables for algorithm:

    // current clusters: full dendrograms/trees
    var currentTrees = initTrees.zipWithIndex.map(_.swap).toVector.toMap

    // queue of candidate merges in priority order
    val mergeQueue = Log.traceBranch("Initializing merges") {
      IO {
        val initMerges = currentTrees.toList.tails.flatMap {
          case Nil => Nil
          case (leftIndex, (left, leftParam)) :: rights =>
            rights.iterator.map { case (rightIndex, (right, rightParam)) =>
              val loss = mergeLoss(left, leftParam, right, rightParam)
              val delta = getLossChangePriority(loss, left.loss, leftParam, right.loss, rightParam)
              MergeCandidate(leftIndex, rightIndex, delta, loss)
            }
        }.toVector.sorted
        Log.trace(s"${initMerges.size} initialized.").unsafeRunSync()
        SplayTreeQueue.fromSortedVector(initMerges)
      }
    }.unsafeRunSync()

    // Log.trace(mergeQueue.toString).unsafeRunSync()
    // System.err.println(mergeQueue.toString)

    // counter for number of stale merge candidates remaining for each pair.
    // CAVEAT: a value of 0 means there IS an ACTIVE merge candidate in the queue,
    // at least for remaining values.
    val staleCounts = Array.ofDim[Int](initNumTrees, initNumTrees)

    // union-find structure to identify cluster representatives
    val clusterSets = TarjanUnionFind.empty[Int]
    Log.traceBranch("Initializing clusters")(
      IO(indices.foreach(clusterSets.add))
    ).unsafeRunSync()

    Log.traceBranch("Clustering") {
      IO {
        var done: Boolean = false
        var numMerges: Int = 0
        var numCandidates: Int = 0
        var numTraversed: Long = 0L
        var numSkips: Int = 0

        def logInfo = (
          Log.rewind >> Log.trace(s"""
          |Merges completed:     $numMerges
          |Candidates generated: $numCandidates
          |Items traversed:      $numTraversed
          |Items skipped:        $numSkips
        """.stripMargin.trim)
        ).unsafeRunSync()

        logInfo
        while(!done && currentTrees.size > 1) {
          mergeQueue.popMin() match {
            case None => ??? // should not happen; or, can use this instead of currentTrees.size > 1 condition above?
            case Some((MergeCandidate(origI, origJ, delta, newLoss), popMinOps)) =>
              numTraversed += popMinOps
              val i = clusterSets.find(origI).get
              val j = clusterSets.find(origJ).get
              require(i != j)
              staleCounts(i)(j) match {
                case 0 => // merge
                  done = stoppingCondition(currentTrees, i, j, newLoss)
                  if(!done) {
                    // merge indices in disjoint sets repr, and update staleness counts
                    val newRep = clusterSets.union(i, j).get
                    val oldRep = if(newRep == i) j else i
                    require(Set(newRep, oldRep) == Set(i, j))
                    indices.foreach { k =>
                      // three cases:
                      // 1) k == newRep. then we're on the diagonal; it's meaningless.
                      // 2) k == oldRep. then we're on the just-now-stale 0 we took to get here. meaningless.
                      // 3) k is neither. then we're doing the right thing.
                      // TODO could use a running set of active indices so this update gets shorter each time.
                      val newStaleCounts = {
                        // if there are 0 stales, that means there is an active one,
                        // which is now becoming stale.
                        // otherwise, if there are >0 stales, then there must be none active,
                        // so we only count the stales.
                        math.max(1, staleCounts(newRep)(k)) +
                          math.max(1, staleCounts(oldRep)(k))
                      }
                      staleCounts(newRep)(k) = newStaleCounts
                      staleCounts(k)(newRep) = newStaleCounts
                    }
                    // perform the actual merge
                    val (leftTree, leftParam) = currentTrees(i)
                    val (rightTree, rightParam) = currentTrees(j)
                    val newTree = MergeTree.Merge(newLoss, leftTree, rightTree)
                    sanityCheck(newLoss, leftTree, leftParam, rightTree, rightParam)
                    val newParam = mergeParams(leftTree, leftParam, rightTree, rightParam)
                    // update the current set of trees
                    currentTrees --= List(i, j) // remove i and j from current trees
                    currentTrees += (newRep -> (newTree -> newParam))
                    numMerges += 1
                  }
                case 1 => // insert a new merge into the queue
                  staleCounts(i)(j) -= 1
                  staleCounts(j)(i) -= 1
                  // now there are no stale precursor merges left, so this merge can happen
                  val (leftTree, leftParam) = currentTrees(i)
                  val (rightTree, rightParam) = currentTrees(j)
                  val loss = mergeLoss(leftTree, leftParam, rightTree, rightParam)
                  val delta = getLossChangePriority(loss, leftTree.loss, leftParam, rightTree.loss, rightParam)
                  val candidate = MergeCandidate(i, j, delta, loss)
                  // insert it into the appropriate location in the sequence
                  val insertOps = mergeQueue.insert(candidate)
                  numTraversed += insertOps
                  numCandidates += 1
                case _ => // skip
                  staleCounts(i)(j) -= 1
                  staleCounts(j)(i) -= 1
                  numSkips += 1
              }
          }
          logInfo
        }
      }
    }.unsafeRunSync()
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }


  // more efficient implementation! possibly MUCH more efficient for good distance functions
  // easily adaptable into an n^2 logn algorithm for single and complete linkage.
  def runPartialAgglomerativeClustering_SearchBased_List(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean)(
    implicit Log: EphemeralTreeLogger[cats.effect.IO, String]
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    val indices = initTrees.toVector.indices
    val initNumTrees = initTrees.size.toInt

    Log.trace(s"Clustering ${initNumTrees} items.").unsafeRunSync()

    case class MergeCandidate(
      i: Int, j: Int, delta: Double, loss: Double
    )
    object MergeCandidate {
      implicit val mergeCandidateOrdering = Ordering.by[MergeCandidate, Double](_.delta)
    }

    // Variables for algorithm:

    // current clusters: full dendrograms/trees
    var currentTrees = initTrees.zipWithIndex.map(_.swap).toVector.toMap

    // queue of candidate merges in priority order
    var orderedMerges = Log.traceBranch("Initializing merges") {
      IO {
        val initMerges = currentTrees.toList.tails.flatMap {
          case Nil => Nil
          case (leftIndex, (left, leftParam)) :: rights =>
            rights.iterator.map { case (rightIndex, (right, rightParam)) =>
              val loss = mergeLoss(left, leftParam, right, rightParam)
              val delta = getLossChangePriority(loss, left.loss, leftParam, right.loss, rightParam)
              MergeCandidate(leftIndex, rightIndex, delta, loss)
            }
        }.toList.sorted
        Log.trace(s"${initMerges.size} initialized.").unsafeRunSync()
        initMerges
      }
    }.unsafeRunSync()

    // counter for number of stale merge candidates remaining for each pair.
    // CAVEAT: a value of 0 means there IS an ACTIVE merge candidate in the queue,
    // at least for remaining values.
    val staleCounts = Array.ofDim[Int](initNumTrees, initNumTrees)

    // union-find structure to identify cluster representatives
    val clusterSets = TarjanUnionFind.empty[Int]
    Log.traceBranch("Initializing clusters")(
      IO(indices.foreach(clusterSets.add))
    ).unsafeRunSync()

    Log.traceBranch("Clustering") {
      IO {
        var done: Boolean = false
        var numMerges: Int = 0
        var numCandidates: Int = 0
        var numTraversed: Long = 0L
        var numSkips: Int = 0

        def logInfo = (
          Log.rewind >> Log.trace(s"""
          |Merges completed:     $numMerges
          |Candidates generated: $numCandidates
          |Items traversed:      $numTraversed
          |Items skipped:        $numSkips
        """.stripMargin.trim)
        ).unsafeRunSync()

        logInfo
        while(!done && currentTrees.size > 1) {
          orderedMerges match {
            case Nil => ??? // should not happen; or, can use this instead of currentTrees.size > 1 condition above?
            case MergeCandidate(origI, origJ, delta, newLoss) :: rest =>
              val i = clusterSets.find(origI).get
              val j = clusterSets.find(origJ).get
              // if(i == j) { // already merged implicitly; skip
              //   orderedMerges = rest
              // } else
              require(i != j)
              staleCounts(i)(j) match {
                case 0 => // merge
                  done = stoppingCondition(currentTrees, i, j, newLoss)
                  if(!done) {
                    // merge indices in disjoint sets repr, and update staleness counts
                    val newRep = clusterSets.union(i, j).get
                    val oldRep = if(newRep == i) j else i
                    require(Set(newRep, oldRep) == Set(i, j))
                    indices.foreach { k =>
                      // three cases:
                      // 1) k == newRep. then we're on the diagonal; it's meaningless.
                      // 2) k == oldRep. then we're on the just-now-stale 0 we took to get here. meaningless.
                      // 3) k is neither. then we're doing the right thing.
                      // TODO could use a running set of active indices so this update gets shorter each time.
                      val newStaleCounts = {
                        // if there are 0 stales, that means there is an active one,
                        // which is now becoming stale.
                        // otherwise, if there are >0 stales, then there must be none active,
                        // so we only count the stales.
                        math.max(1, staleCounts(newRep)(k)) +
                          math.max(1, staleCounts(oldRep)(k))
                      }
                      staleCounts(newRep)(k) = newStaleCounts
                      staleCounts(k)(newRep) = newStaleCounts
                    }
                    // perform the actual merge
                    val (leftTree, leftParam) = currentTrees(i)
                    val (rightTree, rightParam) = currentTrees(j)
                    val newTree = MergeTree.Merge(newLoss, leftTree, rightTree)
                    sanityCheck(newLoss, leftTree, leftParam, rightTree, rightParam)
                    val newParam = mergeParams(leftTree, leftParam, rightTree, rightParam)
                    // update the current set of trees
                    currentTrees --= List(i, j) // remove i and j from current trees
                    currentTrees += (newRep -> (newTree -> newParam))
                    orderedMerges = rest
                    numMerges += 1
                  }
                case 1 => // insert a new merge into the queue
                  staleCounts(i)(j) -= 1
                  staleCounts(j)(i) -= 1
                  // now there are no stale precursor merges left, so this merge can happen
                  val (leftTree, leftParam) = currentTrees(i)
                  val (rightTree, rightParam) = currentTrees(j)
                  val loss = mergeLoss(leftTree, leftParam, rightTree, rightParam)
                  val delta = getLossChangePriority(loss, leftTree.loss, leftParam, rightTree.loss, rightParam)
                  val candidate = MergeCandidate(i, j, delta, loss)
                  // insert it into the appropriate location in the sequence
                  orderedMerges = {
                    val (prefix, suffix) = rest.span(_.delta < delta)
                    numTraversed += prefix.size
                    prefix ++ (candidate :: suffix)
                  }
                  numCandidates += 1
                case _ => // skip
                  staleCounts(i)(j) -= 1
                  staleCounts(j)(i) -= 1
                  orderedMerges = rest
                  numSkips += 1
              }
          }
          logInfo
        }
      }
    }.unsafeRunSync()
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }

  // less efficient queue-based implementation.
  def runPartialAgglomerativeClustering_ManyQueues(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    val indices = initTrees.toVector.indices
    var currentTrees = initTrees.zipWithIndex.map(_.swap).toVector.toMap

    val distances = Array.ofDim[Double](initTrees.size.toInt, initTrees.size.toInt)

    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      indices.map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(left, leftParam, right, rightParam)
          val delta = getLossChangePriority(loss, left.loss, leftParam, right.loss, rightParam)
          distances(leftIndex)(rightIndex) = delta
          distances(rightIndex)(leftIndex) = delta
          queues(leftIndex) += (rightIndex -> loss)
        }
    }

    var done: Boolean = false
    while(!done && currentTrees.size > 1) {
      val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
      val (j, newLoss) = queues(i).head
      done = stoppingCondition(currentTrees, i, j, newLoss)
      if(!done) {
        queues -= j // remove j from consideration
        queues(i).clear() // clear i's queue since we'll refresh it
        val emptyQueues = queues.iterator.flatMap { case (k, q) =>
          q --= List(i, j)  // remove stale entries for i and j from all queues
          if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
        }
        queues --= emptyQueues // and remove other now-empty queues
        val (leftTree, leftParam) = currentTrees(i)
        val (rightTree, rightParam) = currentTrees(j)
        val newTree = MergeTree.Merge(newLoss, leftTree, rightTree)
        sanityCheck(newLoss, leftTree, leftParam, rightTree, rightParam)
        val newParam = mergeParams(leftTree, leftParam, rightTree, rightParam)
        currentTrees --= List(i, j) // remove i and j from current trees
                                    // get new merge candidates before adding new cluster back in
        val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
          val loss = mergeLoss(newTree, newParam, right, rightParam)
          val delta = getLossChangePriority(loss, newTree.loss, newParam, right.loss, rightParam)
          // update distances before inserting anything into queues to keep things consistent
          distances(i)(k) = delta
          distances(k)(i) = delta
          k -> loss
        }.toList
        currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
        queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
      }

    }
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }

  def finishAgglomerativeClustering(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): (MergeTree[Index], ClusterParam) = {
    runPartialAgglomerativeClustering(
      initTrees,
      (_, _, _, _) => false
    ).head
  }

  def runAgglomerativeClustering(
    indices: NonEmptyVector[Index],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {

    val initTrees = indices.map { index =>
      val param = getSingleInstanceParameter(index)
      val loss = getInstanceLoss(index, param)
      (MergeTree.Leaf(loss, index): MergeTree[Index]) -> param
    }

    runPartialAgglomerativeClustering(initTrees, stoppingCondition)
  }

  def runFullAgglomerativeClustering(
    instances: NonEmptyVector[Index])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): (MergeTree[Index], ClusterParam) = {
    runAgglomerativeClustering(
      instances,
      (_, _, _, _) => false
    ).head
  }

  def sanityCheck(
    loss: Double,
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam) = {
    // need the tolerance here because of Float precision issues
    if(!(loss >= (left.loss + right.loss - 1e-4))) {
      System.err.println("WARNING: clusters seem to be incorrectly merged")
      System.err.println(left)
      System.err.println(leftParam)
      // System.err.println(aggregateLosses(indices.map(getInstanceLoss(_, leftParam))))
      System.err.println(right)
      System.err.println(rightParam)
      // System.err.println(aggregateLosses(indices.map(getInstanceLoss(_, rightParam))))
      System.err.println(loss)
      ???
    }
  }

  // TODO: could change merge trees to store ranks, and then keep the original rank in the filtered tree?
  // maybe this is fine...
  def filterAndReMerge(tree: MergeTree[Index], f: Index => Option[Index]): Option[MergeTree[Index]] = {
    tree.cata[Option[(MergeTree[Index], ClusterParam)]](
      leaf = (loss, oldI) => f(oldI).map { newI =>
        val param = getSingleInstanceParameter(newI)
        val loss = getInstanceLoss(newI, param)
        MergeTree.Leaf(loss, newI) -> param
      },
      merge = (loss, leftOpt, rightOpt) => (leftOpt, rightOpt) match {
        case (l, None) => l
        case (None, r) => r
        case (Some((leftTree, leftParam)), Some((rightTree, rightParam))) =>
          val loss = mergeLoss(leftTree, leftParam, rightTree, rightParam)
          val param = mergeParams(leftTree, leftParam, rightTree, rightParam)
          Some(MergeTree.Merge(loss, leftTree, rightTree) -> param)
      }
    ).map(_._1)
  }



  // standalone implementation for paper; wip
  def runAgglomerativeClusteringStandalone(
    inputs: NonEmptyVector[Index],
    delta: (MergeTree[Index], MergeTree[Index]) => Double
  ): NonEmptyVector[MergeTree[Index]] = {
    case class MergeCandidate(i: Int, j: Int, delta: Double)
    object MergeCandidate {
      implicit val mergeCandidateOrdering = Ordering.by[MergeCandidate, Double](_.delta)
      implicit val mergeCandidateOrder = Order.by[MergeCandidate, Double](_.delta)
    }

    val initTrees = inputs.map { index =>
      (MergeTree.Leaf(0.0, index): MergeTree[Index])
    }
    val indices = initTrees.toVector.indices
    val initNumTrees = initTrees.size.toInt

    // Variables for algorithm:

    // Clusters so far: full dendrograms/hierarchical cluster trees
    var currentTrees: Map[Int, MergeTree[Index]] =
      initTrees.zipWithIndex.map(_.swap).toVector.toMap

    // queue of candidate merges in priority order
    val mergeQueue = {
      val initMerges = currentTrees.toList.tails.flatMap {
        case Nil => Nil
        case (leftIndex, left) :: rights =>
          rights.iterator.map { case (rightIndex, right) =>
            MergeCandidate(leftIndex, rightIndex, delta(left, right))
          }
      }.toVector.sorted
      SplayTreeQueue.fromSortedVector(initMerges)
    }

    // counter for number of stale merge candidates remaining for each pair.
    // CAVEAT: a value of 0 means there IS an ACTIVE merge candidate in the queue,
    // at least for active pairs of values.
    val staleCounts = Array.ofDim[Int](initNumTrees, initNumTrees)

    // union-find structure to identify cluster representatives
    val clusterSets = TarjanUnionFind.empty[Int]
    indices.foreach(clusterSets.add)

    while(currentTrees.size > 1) {
      mergeQueue.popMin().get._1 match {
        case MergeCandidate(origI, origJ, _) =>
          // get cluster representatives for merge candidates
          val i = clusterSets.find(origI).get
          val j = clusterSets.find(origJ).get
          staleCounts(i)(j) match {
            case 0 => // execute a merge

              // new representative index for the resulting cluster: will be one of i or j
              val newRep = clusterSets.union(i, j).get

              // the representative index that we're losing: will be the other one
              val oldRep = if(newRep == i) j else i
              // is no longer active: we don't need to update stale counts

              // get trees to be merged
              val leftTree = currentTrees(i)
              val rightTree = currentTrees(j)
              // remove i and j from current trees
              currentTrees --= List(i, j)

              // update stale counts for the new cluster and all other remaining clusters
              currentTrees.keySet.foreach { k =>
                // number now stale in queue is what's recorded in the matrix,
                // UNLESS it was 0, which meant there was 1 *active* in the queue,
                // which means there is now 1 newly stale in the queue.
                val newStaleCounts = math.max(1, staleCounts(k)(i)) + math.max(1, staleCounts(k)(j))
                staleCounts(newRep)(k) = newStaleCounts
                staleCounts(k)(newRep) = newStaleCounts
              }
              // perform the actual merge
              val newTree = MergeTree.Merge(0.0, leftTree, rightTree)
              // update the current set of trees
              currentTrees += (newRep -> newTree)
            case 1 => // insert a new merge into the queue
              staleCounts(i)(j) -= 1
              staleCounts(j)(i) -= 1
              // now there are no stale precursor merges left, so this merge can happen
              val left = currentTrees(i)
              val right = currentTrees(j)
              val candidate = MergeCandidate(i, j, delta(left, right))
              // insert it into the queue
              mergeQueue.insert(candidate)
            case _ => // skip
              staleCounts(i)(j) -= 1
              staleCounts(j)(i) -= 1
          }
      }
    }
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }

}
