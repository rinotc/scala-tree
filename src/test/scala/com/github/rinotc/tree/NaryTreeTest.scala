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


  describe("filter") {
    it("条件を満たすNodeを取得する") {
      val actual = naryTree.filter(_ % 2 == 1)
      actual should contain theSameElementsAs List(1, 3, 5, 7)
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
}
