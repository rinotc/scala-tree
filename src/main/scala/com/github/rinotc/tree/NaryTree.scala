package com.github.rinotc.tree

import scala.annotation.tailrec

/**
 * 多分木
 */
final case class NaryTree[Node](node: Node, children: List[NaryTree[Node]]) {

  def find(f: Node => Boolean): Option[NaryTree[Node]] = {
    @tailrec
    def loop(nodes: List[NaryTree[Node]], stack: List[NaryTree[Node]]): Option[NaryTree[Node]] = {
      nodes match {
        case Nil => stack.headOption
        case ::(head, next) =>
          if (f(head.node)) Some(head)
          else loop(head.children ::: next, stack)
      }
    }

    loop(children, List(this))
  }

  def filter(f: Node => Boolean): List[NaryTree[Node]] = {
    @tailrec
    def loop(nodes: List[NaryTree[Node]], acc: List[NaryTree[Node]]): List[NaryTree[Node]] = nodes match {
      case Nil => acc
      case ::(head, next) =>
        val newAcc = if (f(head.node)) head :: acc else acc
        loop(next ++ head.children, newAcc)
    }

    loop(children, List(this))
  }

  /**
   * 条件を満たす要素の数を返す
   */
  def countIf(f: Node => Boolean): Int = flatten.count(f)

  def height: Int = {
    if (children.isEmpty) 0 // 子がない場合は高さは0
    else {
      val childHeights = children.map(_.height) // 子の高さを再帰的に計算
      1 + childHeights.max // 子の最大高さに自身のノードを追加した高さを返す
    }
  }

  def ascendantsOf(f: Node => Boolean): List[Node] = ???

  def flatten: List[Node] = {
    @tailrec
    def loop(nodes: List[NaryTree[Node]], acc: List[Node]): List[Node] = nodes match {
      case Nil => acc
      case ::(head, next) =>
        loop(head.children ++ next, head.node :: acc)
    }

    loop(children, List(this.node))
  }

  def map[B](f: Node => B): NaryTree[B] = {
    def loop(nodes: List[NaryTree[Node]], acc: List[NaryTree[B]]): List[NaryTree[B]] = {
      nodes match {
        case Nil => acc
        case ::(head, next) =>
          loop(next, NaryTree(f(head.node), loop(head.children, List.empty)) :: acc)
      }
    }

    NaryTree(f(node), loop(children, List.empty))
  }
}

object NaryTree {

  def buildFrom[Node <: AdjacencyListElement](adjacencyList: List[Node]): NaryTree[Node] = {
    val rootNodes = adjacencyList.filter(_.parent.isEmpty)

    def buildTree(node: Node): NaryTree[Node] = {
      val children = adjacencyList.collect {
        case child if child.parent.contains(node.data) => buildTree(child)
      }
      NaryTree(node, children)
    }

    rootNodes.headOption match {
      case Some(root) => buildTree(root)
      case None => throw new IllegalArgumentException("No root node found.")
    }
  }
}