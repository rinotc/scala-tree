package com.github.rinotc.tree

/**
 * 隣接リスト
 */
trait AdjacencyListElement {

  type A
  def data: A
  def parent: Option[A]
}
