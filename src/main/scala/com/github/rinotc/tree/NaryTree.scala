package com.github.rinotc.tree


import scala.annotation.tailrec

/**
 * N分木
 */
final case class NaryTree[Node](node: Node, children: List[NaryTree[Node]]) {

  def find(f: Node => Boolean): Option[NaryTree[Node]] = flatTree.find(t => f(t.node))

  /**
   * 条件を満たすNaryTreeのリストを取得する
   *
   * @example
   * f: num % 2 == 1
   * {{{
   *      1            1          3      5
   *     / \          / \        / \
   *    2   3    ->  2   3      4   5
   *       / \          / \
   *      4   5        4   5,        ,
   * }}}
   */
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

  /**
   * 木を全てflatにする.
   *
   * @example
   * {{{
   *    1            1         2        3       4     5
   *   / \          / \                / \
   *  2   3    ->  2   3              4   5
   *     / \          / \
   *    4   5        4   5,        ,        ,      ,
   * }}}
   */
  def flatTree: List[NaryTree[Node]] = {
    @tailrec
    def loop(nodes: List[NaryTree[Node]], acc: List[NaryTree[Node]]): List[NaryTree[Node]] = nodes match {
      case Nil => acc
      case ::(head, next) =>
        loop(head.children ++ next, head :: acc)
    }

    loop(children, List(this))
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

  /**
   * 条件を満たす子ノード以下のみの木を再構築する.
   * @note 上位のノードが条件を満たさない場合は、それより下のノードが条件を満たしていたとしても
   * 木には含まれない。
   * @example
   * f: node % 2 == 0
   * {{{
   *       1               1
   *    /  \  \           / \
   *   2    7  10  ->   2    10
   *  / \    \           \
   * 3   4    8           4
   *    / \                \
   *   5   6                6
   * }}}
   */
  def prune(f: Node => Boolean): NaryTree[Node] = {
    val cs = this.children.collect {
      case child if f(child.node) => child.prune(f)
    }
    NaryTree(this.node, cs)
  }
}

object NaryTree {

  /**
   * ルートノードを一つもつ隣接リスト要素のリストから、N分木を構築する
   *
   * @param adjacencyList 隣接リスト. 親要素を持たないレコードが一つだけ存在する必要がある。
   * @tparam Node [[AdjacencyListElement]] を継承しているデータ型
   * @return ルートノードからの[[NaryTree]]
   */
  def buildFromRoot[Node <: AdjacencyListElement](adjacencyList: List[Node]): NaryTree[Node] = {
    val rootNodes = adjacencyList.filter(_.parent.isEmpty)
    require(rootNodes.lengthIs == 1, "root nodes must be only 1.")

    def buildTree(node: Node): NaryTree[Node] = {
      val children = adjacencyList.collect {
        case child if child.parent.contains(node.data) => buildTree(child)
      }
      NaryTree(node, children)
    }

    buildTree(rootNodes.head) // requireでルートノードが一つだけであることを制約済み
  }

  /**
   * 隣接リストから、[[NaryTree]] のリストを構築する
   */
  def buildTreeList[Node <: AdjacencyListElement](adjacencyList: List[Node]): List[NaryTree[Node]] = {

    val parentEmptyNodes = adjacencyList.filter(_.parent.isEmpty)
    if (parentEmptyNodes.lengthIs == 1) List(buildFromRoot(adjacencyList)) // ルートノードが一つのみ
    else if (parentEmptyNodes.lengthIs > 1) { // ルートノードが複数ある
      def constructTree(data: Node#A): NaryTree[Node] = {
        val children = adjacencyList.filter(_.parent.contains(data))
          .map(node => constructTree(node.data))
        NaryTree(adjacencyList.find(_.data == data).get, children)
      }

      parentEmptyNodes.map(node => constructTree(node.data))
    } else { // ルートノードがリストに含まれてない（部分木）
      val nodeMap: Map[Node#A, List[Node]] = adjacencyList.groupBy(_.parent.get)
      val rootNodes = adjacencyList.flatMap(_.parent).toSet intersect adjacencyList.map(_.data).toSet

      def constructTree(data: Node#A): NaryTree[Node] = {
        val children = nodeMap.getOrElse(data, Nil).map(child => constructTree(child.data))
        NaryTree(adjacencyList.find(_.data == data).get, children)
      }

      rootNodes.map(constructTree).toList
    }
  }
}