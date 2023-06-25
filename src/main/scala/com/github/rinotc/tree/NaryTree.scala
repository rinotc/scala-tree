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