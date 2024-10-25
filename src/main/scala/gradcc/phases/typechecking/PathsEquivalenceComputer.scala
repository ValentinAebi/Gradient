package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.lang.{Path, RecordField, SelectPath, VarPath}
import gradcc.phases.typechecking.PathsEquivalenceComputer.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer as MutList, Map as MutMap, Set as MutSet}


final class PathsEquivalenceComputer private(
                                              private val vars: MutMap[UniqueVarId, NodeId],
                                              private val nodes: MutMap[NodeId, Node]
                                            ) {

  def deepCopy: PathsEquivalenceComputer =
    PathsEquivalenceComputer(this.vars.map(identity), this.nodes.map((nid, n) => (nid, n.deepCopy)))

  def assertEquivalent(p: Path, q: Path): Unit = {
    val pNode = nodes(findOrCreateNodeAndParents(p))
    val qNode = nodes(findOrCreateNodeAndParents(q))
    unify(pNode, qNode)
  }

  def assertSelectEquiv(owner: Path, select: RecordField, value: Path): Unit =
    assertEquivalent(SelectPath(owner, select), value)

  def getEquivClass(id: UniqueVarId): Set[UniqueVarId] = {
    val targetNode = nodes(findOrCreateNodeAndParents(VarPath(id)))
    vars.filter((_, nid) => nodes(nid) == targetNode).keySet.toSet
  }

  def expressAsPathFrom(origin: Path, target: Path): Option[Path] = {

    val originId = findOrCreateNodeAndParents(origin)
    val targetId = findOrCreateNodeAndParents(target)
    val targetNode = nodes(targetId)

    val searchQueue = mutable.Queue.empty[(NodeId, Path)]
    val alreadyEnqueued = MutSet.empty[NodeId]

    def enq(nodeId: NodeId, p: Path): Unit = {
      searchQueue.addOne((nodeId, p))
      alreadyEnqueued.addOne(nodeId)
    }

    enq(originId, origin)
    while (searchQueue.nonEmpty) {
      val (startId, p) = searchQueue.dequeue()
      if (targetNode.referringIds.contains(startId)) {
        return Some(p)
      }
      for ((fld, nodeId) <- nodes(startId).fields if !alreadyEnqueued.contains(nodeId)) {
        enq(nodeId, SelectPath(p, fld))
      }
    }

    None
  }

  /**
   * Merge n2 into n1
   */
  private def unify(n1: Node, n2: Node): Unit = {
    for (fld <- n1.fields.keys ++ n2.fields.keys) {
      val subN1 = n1.fields.get(fld)
      val subN2 = n2.fields.get(fld)
      (subN1, subN2) match {
        case (None, None) => assert(false)
        case (Some(r1), None) => ()
        case (None, Some(r2)) =>
          n1.fields(fld) = r2
        case (Some(r1), Some(r2)) =>
          unify(nodes(r1), nodes(r2))
      }
    }
    n1.referringIds ++= n2.referringIds
    for (id <- n2.referringIds) {
      nodes(id) = n1
    }
  }

  private def findOrCreateNodeAndParents(p: Path): NodeId = p match {
    case VarPath(root) =>
      vars.getOrElse(root, {
        val nodeId = nextId()
        val node = Node(MutMap.empty, MutSet(nodeId))
        nodes(nodeId) = node
        vars(root) = nodeId
        nodeId
      })
    case SelectPath(lhs, field) =>
      val parentId = findOrCreateNodeAndParents(lhs)
      nodes(parentId).fields.getOrElse(field, {
        val parentNode = nodes(parentId)
        val nodeId = nextId()
        val node = Node(MutMap.empty, MutSet(nodeId))
        nodes(nodeId) = node
        parentNode.fields(field) = nodeId
        nodeId
      })
  }

  override def toString: String = {
    val alreadyDisplayed = MutSet.empty[Node]
    vars.groupBy(_._2).map { (nodeId, uniqueVarIdsToNodeIds) =>
      val node = nodes(nodeId)
      alreadyDisplayed += node
      uniqueVarIdsToNodeIds.keys.mkString(",") + " : " + node
    }.mkString("\n") + "\n" +
      (nodes.values.toSet -- alreadyDisplayed).mkString("\n")
  }

}

object PathsEquivalenceComputer {

  def empty: PathsEquivalenceComputer = PathsEquivalenceComputer(MutMap.empty, MutMap.empty)

  private type NodeId = Int

  private val idsGenerator = AtomicInteger(0)

  private def nextId(): NodeId = idsGenerator.incrementAndGet()

  private class Node(
                      val fields: MutMap[RecordField, NodeId],
                      val referringIds: MutSet[NodeId]
                    ) {

    def deepCopy: Node = Node(
      fields.map(identity),
      referringIds.map(identity)
    )

    override def toString: String =
      referringIds.mkString(",") + " : "
        + fields.toSeq
        .sortBy(_.toString)
        .map(_.toString + ":" + _)
        .mkString(" {", ",", "}")
  }

}
