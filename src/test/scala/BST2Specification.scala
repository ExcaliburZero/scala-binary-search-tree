import org.scalacheck.Properties
import org.scalacheck.Prop._

object BST2Specification extends Properties("BinarySearchTree2") {

  property("emptySearch") = forAll { (a: Int) =>
    !Empty().search(a)
  }

  property("insertSearch") = forAll { (a: Int) =>
    Empty().insert(a).search(a)
  }

  property("insertCollect") = forAll { (items: List[Int]) =>
    Empty().insertMultiple(items).collect == items.sorted
  }

  property("mapCollect") = forAll { (f: (Int => Int), items: List[Int]) =>
    Empty().insertMultiple(items).map(f).collect == items.map(f).sorted
  }

  property("insertSize") = forAll { (items: List[Int]) =>
    Empty().insertMultiple(items).size == items.size
  }

}