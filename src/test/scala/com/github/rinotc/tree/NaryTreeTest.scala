package com.github.rinotc.tree

object NaryTreeTest {
  case class AdjEle(data: Int, parent: Option[Int]) extends AdjacencyListElement {
    type A = Int
  }
}

class NaryTreeTest extends BaseTest {

  import NaryTreeTest._

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
      NaryTree(7, List(
        NaryTree(8, List.empty)
      ))
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
      val expected = List(
        NaryTree(
          1,
          List(
            NaryTree(2, List(
              NaryTree(3, List.empty),
              NaryTree(4, List(
                NaryTree(5, List.empty),
                NaryTree(6, List.empty)
              ))
            )),
            NaryTree(7, List(NaryTree(8, List.empty)))
          )
        ),
        NaryTree(3, List.empty),
        NaryTree(5, List.empty),
        NaryTree(7, List(
          NaryTree(8, List.empty)
        ))
      )
      actual.length shouldBe 4
      actual should contain theSameElementsAs expected
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

  describe("findPath") {
    it("条件を満たす要素までのルートノードからの経路を取得する") {
      val actual = naryTree.findPath(_ == 5).value
      val expected = List(1, 2, 4, 5)
      actual should contain theSameElementsInOrderAs expected // 順序も保証する
    }

    it("条件を満たす要素が存在しない場合はNoneを返す") {
      val actual = naryTree.findPath(_ == 99)
      actual shouldBe None
    }
  }

  describe("flatten") {
    it("要素をフラットにする") {
      val actual = naryTree.flatten
      actual should contain theSameElementsAs List(1, 2, 3, 4, 5, 6, 7, 8)
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
      val expected = NaryTree(
        2,
        List(
          NaryTree(4, List(
            NaryTree(6, List.empty),
            NaryTree(8, List(
              NaryTree(10, List.empty),
              NaryTree(12, List.empty)
            ))
          )),
          NaryTree(14, List(
            NaryTree(16, List.empty)
          ))
        )
      )
      actual shouldBe expected
    }
  }


  describe("prune") {
    it("条件を満たすnodeだけのツリーを作る。rootNodeはその条件に含まない。") {
      val nTree: NaryTree[Int] = NaryTree(
        1,
        List(
          NaryTree(2, List(
            NaryTree(3, List.empty),
            NaryTree(4, List(
              NaryTree(5, List.empty),
              NaryTree(6, List.empty)
            ))
          )),
          NaryTree(7, List(
            NaryTree(8, List.empty), // 上位のノードが条件を満たさない場合は、下のノードが条件を満たしても木には含まれない
            NaryTree(9, List.empty)
          )),
          NaryTree(10, List.empty)
        )
      )
      val actual = nTree.prune(_ % 2 == 0)
      val expected = NaryTree(
        1,
        List(
          NaryTree(2, List(
            NaryTree(4, List(
              NaryTree(6, List.empty)
            ))
          )),
          NaryTree(10, List.empty)
        )
      )

      assert(actual == expected)
    }
  }


  describe("buildFromRoot") {
    it("隣接リストから多分木を構築する") {

      val e1 = AdjEle(1, None)
      val e2 = AdjEle(2, Some(1))
      val e3 = AdjEle(3, Some(1))
      val e4 = AdjEle(4, Some(2))
      val e5 = AdjEle(5, Some(2))
      val e6 = AdjEle(6, Some(3))
      val e7 = AdjEle(7, Some(6))
      val list = List(e1, e2, e3, e4, e5, e6, e7)

      val actual: NaryTree[AdjEle] = NaryTree.buildFromRoot(list)

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

  describe("buildTreeList") {
    it("ルートが一つだけ存在する隣接リストから、N分木を構築する") {
      val e1 = AdjEle(1, None)
      val e2 = AdjEle(2, Some(1))
      val e3 = AdjEle(3, Some(1))
      val e4 = AdjEle(4, Some(2))
      val e5 = AdjEle(5, Some(2))
      val e6 = AdjEle(6, Some(3))
      val e7 = AdjEle(7, Some(6))
      val list = List(e1, e2, e3, e4, e5, e6, e7)

      val actual: List[NaryTree[AdjEle]] = NaryTree.buildTreeList(list)

      val expected = List(
        NaryTree(
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
      )
      actual should contain theSameElementsAs expected
    }

    it("ルートノードが複数存在する、つまり、parentがNoneのNodeが複数存在する隣接リストからN分木を構築する") {
      val a0 = AdjEle(10, None)
      val a1 = AdjEle(11, Some(10))
      val a2 = AdjEle(12, Some(10))
      val a3 = AdjEle(13, Some(10))
      val a4 = AdjEle(14, Some(11))
      val a5 = AdjEle(15, Some(12))
      val a6 = AdjEle(16, Some(12))
      val a7 = AdjEle(17, Some(13))
      val a8 = AdjEle(18, Some(13))
      val aList = List(a0, a1, a2, a3, a4, a5, a6, a7, a8)

      val b0 = AdjEle(20, None)
      val b1 = AdjEle(21, Some(20))
      val b2 = AdjEle(22, Some(20))
      val b3 = AdjEle(23, Some(22))
      val b4 = AdjEle(24, Some(22))
      val b5 = AdjEle(25, Some(22))
      val bList = List(b0, b1, b2, b3, b4, b5)

      val c0 = AdjEle(30, None)
      val c1 = AdjEle(31, Some(30))
      val cList = List(c0, c1)

      val actual = NaryTree.buildTreeList(aList ++ bList ++ cList)

      val expected = List(
        NaryTree(a0, List(
          NaryTree(a1, List(NaryTree(a4, List.empty))),
          NaryTree(a2, List(NaryTree(a5, List.empty), NaryTree(a6, List.empty))),
          NaryTree(a3, List(NaryTree(a7, List.empty), NaryTree(a8, List.empty))),
        )),
        NaryTree(b0, List(
          NaryTree(b1, List()),
          NaryTree(b2, List(
            NaryTree(b3, List.empty),
            NaryTree(b4, List.empty),
            NaryTree(b5, List.empty)
          ))
        )),
        NaryTree(c0, List(NaryTree(c1, List.empty)))
      )

      actual.length shouldBe 3
      actual should contain theSameElementsAs expected
      actual.flatMap(_.flatten) should contain theSameElementsAs aList ++ bList ++ cList
    }

    it("ルートノード、つまりparentがNoneのレコードが存在しない場合") {
      val a1 = AdjEle(11, Some(10))
      val a2 = AdjEle(12, Some(10))
      val a3 = AdjEle(13, Some(10))
      val a4 = AdjEle(14, Some(11))
      val a5 = AdjEle(15, Some(12))
      val a6 = AdjEle(16, Some(12))
      val a7 = AdjEle(17, Some(13))
      val a8 = AdjEle(18, Some(13))
      val aList = List(a1, a2, a3, a4, a5, a6, a7, a8)

      val actual = NaryTree.buildTreeList(aList)

      val expected = List(
        NaryTree(a1, List(NaryTree(a4, List.empty))),
        NaryTree(a2, List(NaryTree(a5, List.empty), NaryTree(a6, List.empty))),
        NaryTree(a3, List(NaryTree(a7, List.empty), NaryTree(a8, List.empty))),
      )

      actual should contain theSameElementsAs expected
    }
  }
}

