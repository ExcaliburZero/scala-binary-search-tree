import Ordering.Implicits._

sealed trait BinarySearchTree2[A] {
  def insert(v: A)(implicit ev: Ordering[A]): BinarySearchTree2[A] = {
    this match {
      case Empty() => Branch(v, Empty(), Empty())
      case Branch(value, left, right) =>
        if (v <= value) {
          left match {
            case Empty() => Branch(value, Branch(v, Empty(), Empty()), right)
            case Branch(_, _, _) => Branch(value, left.insert(v), right)
          }
        } else {
          right match {
            case Empty() => Branch(value, left, Branch(v, Empty(), Empty()))
            case Branch(_, _, _) => Branch(value, left, right.insert(v))
          }
        }
    }
  }
}
case class Empty[A]() extends BinarySearchTree2[A]
case class Branch[A](value: A, left: BinarySearchTree2[A], right: BinarySearchTree2[A]) extends BinarySearchTree2[A]