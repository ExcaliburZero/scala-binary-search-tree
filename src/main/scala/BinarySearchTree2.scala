import Ordering.Implicits._
import scala.annotation.tailrec

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

  def insertMultiple(values: List[A])(implicit ev: Ordering[A]): BinarySearchTree2[A] = {
    values.foldLeft(this)((tree, v) => tree.insert(v))
  }

  def size(): Int = {
    this match {
      case Empty() => 0
      case Branch(_, left, right) => left.size() + right.size() + 1
    }
  }

  @tailrec
  final def search(v: A)(implicit ev: Ordering[A]): Boolean = {
    this match {
      case Empty() => false
      case Branch(value, left, right) =>
        if (v == value) {
          true
        } else if (v < value) {
          left.search(v)
        } else {
          right.search(v)
        }
    }
  }

  def collect(): List[A] = {
    this match {
      case Empty() => List()
      case Branch(value, left, right) =>
        left.collect() ++ List(value) ++ right.collect()
    }
  }

  def map[B](f: A => B)(implicit ev: Ordering[B]): BinarySearchTree2[B] = {
    val currentItems = this.collect()
    val newItems = currentItems.map(f)
    Empty().insertMultiple(newItems)
  }
}
case class Empty[A]() extends BinarySearchTree2[A]
case class Branch[A](value: A, left: BinarySearchTree2[A], right: BinarySearchTree2[A]) extends BinarySearchTree2[A]