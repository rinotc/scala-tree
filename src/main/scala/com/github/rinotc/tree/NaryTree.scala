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
    loop(children, List.empty)
  }
}

object NaryTree {

  def apply[Node](node: Node, children: List[NaryTree[Node]]) = new NaryTree[Node](node, children)
}