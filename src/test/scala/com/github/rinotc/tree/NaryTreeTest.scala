package com.github.rinotc.tree

class NaryTreeTest extends BaseTest {

  val naryTree: NaryTree[Int] = NaryTree(
    1,
    List(
      NaryTree(2, List(
        NaryTree(3, List.empty),
        NaryTree(4, List(
          NaryTree(5, List.empty),
          NaryTree(6, List.empty)
        ))
      )),
      NaryTree(7, List.empty)
    )
  )

  describe("find") {
    it("条件を満たすTreeを取得する") {
      val actual = naryTree.find(_ == 2).value
      val expected = NaryTree(2, List(
        NaryTree(3, List.empty),
        NaryTree(4, List(
          NaryTree(5, List.empty),
          NaryTree(6, List.empty)
        ))
      ))
      assert(actual == expected)
    }
  }

  describe("filter") {
    it("条件を満たすNodeを持つTreeを取得する") {
      val actual = naryTree.filter(_ % 2 == 1)
      actual.map(_.node) should contain theSameElementsAs List(1, 3, 5, 7)
    }
  }

  describe("flatten") {
    it("要素をフラットにする") {
      val actual = naryTree.flatten
      actual should contain theSameElementsAs List(1, 2, 3, 4, 5, 6, 7)
    }
  }

  describe("map") {
    it("全ての要素に関数を適用する") {
      val actual = naryTree.map(_ * 2)
      actual.flatten should contain theSameElementsAs List(2, 4, 6, 8, 10, 12, 14)
    }
  }

  describe("buildFrom") {
    case class AdjEle(data: Int, parent: Option[Int]) extends AdjacencyListElement {
      type A = Int
    }

    it("隣接リストから多分木を構築する") {

      val e1 = AdjEle(1, None)
      val e2 = AdjEle(2, Some(1))
      val e3 = AdjEle(3, Some(1))
      val e4 = AdjEle(4, Some(2))
      val e5 = AdjEle(5, Some(2))
      val e6 = AdjEle(6, Some(3))
      val e7 = AdjEle(7, Some(6))
      val list = List(e1, e2, e3, e4, e5, e6, e7)

      val actual: NaryTree[AdjEle] = NaryTree.buildFrom(list)

      val expected = NaryTree(
        e1,
        List(
          NaryTree(e2, List(
            NaryTree(e4, List()),
            NaryTree(e5, List()),
          )),
          NaryTree(e3, List(
            NaryTree(e6, List(
              NaryTree(e7, List())
            ))
          ))
        )
      )
      assert(actual == expected)
    }
  }
}
