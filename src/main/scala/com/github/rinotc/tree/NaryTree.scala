package com.github.rinotc.tree

import scala.annotation.tailrec

/**
 * 多分木
 */
class NaryTree[Node](val node: Node, val children: List[NaryTree[Node]]) {

  /**
   * 条件を満たす要素のリストを取得する
   */
  def filter(f: Node => Boolean): List[Node] = {
    @tailrec
    def loop(nodes: List[NaryTree[Node]], acc: List[Node]): List[Node] = nodes match {
      case Nil => acc
      case ::(head, next) =>
        val newAcc = if (f(head.node)) head.node :: acc else acc
        loop(next ++ head.children, newAcc)
    }

    loop(children, List(this.node))
  }

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

  def apply[Node](node: Node, children: List[NaryTree[Node]]) = new NaryTree[Node](node, children)
}