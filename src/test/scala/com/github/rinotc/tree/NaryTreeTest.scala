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

  describe("countIf") {
    it("条件を満たす要素の数を返す") {
      val actual = naryTree.countIf(_ % 2 == 1)
      actual shouldBe 4
    }
  }

  describe("height") {
    it("木の高さを返す") {
      naryTree.height shouldBe 3
    }
  }

  describe("descendantsOf") {
    ignore("条件を満たすNodeより祖先のNodeのリストを取得する") {
      val actual = naryTree.ascendantsOf(_ == 4)
      actual should contain theSameElementsAs List(1, 2)
    }
  }

  describe("flatten") {
    it("要素をフラットにする") {
      val actual = naryTree.flatten
      actual should contain theSameElementsAs List(1, 2, 3, 4, 5, 6, 7)
    }
  }

  describe("flatTree") {
    it("木を全て一番上に持ってくる") {
      val t1 = NaryTree(
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
      val t2 = NaryTree(2, List(
        NaryTree(3, List.empty),
        NaryTree(4, List(
          NaryTree(5, List.empty),
          NaryTree(6, List.empty)
        ))
      ))
      val t3 = NaryTree(3, List.empty)
      val t4 = NaryTree(4, List(
        NaryTree(5, List.empty),
        NaryTree(6, List.empty)
      ))
      val t5 = NaryTree(5, List.empty)
      val t6 = NaryTree(6, List.empty)
      val t7 = NaryTree(7, List.empty)

      t1.flatTree.length shouldBe 7
      t1.flatTree should contain theSameElementsAs List(t1, t2, t3, t4, t5, t6, t7)
      t2.flatTree.length shouldBe 5
      t2.flatTree should contain theSameElementsAs List(t2, t3, t4, t5, t6)
      t3.flatTree.length shouldBe 1
      t3.flatTree should contain theSameElementsAs List(t3)
      t4.flatTree.length shouldBe 3
      t4.flatTree should contain theSameElementsAs List(t4, t5, t6)
      t5.flatTree should contain theSameElementsAs List(t5)
      t6.flatTree should contain theSameElementsAs List(t6)
      t7.flatTree should contain theSameElementsAs List(t7)
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
