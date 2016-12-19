package binarysearchtree

case class BinarySearchTree[A <% Ordered[A]](item: A, left: Option[BinarySearchTree[A]], right: Option[BinarySearchTree[A]]) {
  def insert(v: A): BinarySearchTree[A] = {
    if (v <= this.item) {
      this.left match {
        case Some(lTree) => BinarySearchTree[A](this.item, Some(lTree.insert(v)), this.right)
        case None =>
          val newLeft = Some(BinarySearchTree[A](v, None, None))
          BinarySearchTree[A](this.item, newLeft, this.right)
      }
    } else {
      this.right match {
        case Some(rTree) => BinarySearchTree[A](this.item, this.left, Some(rTree.insert(v)))
        case None =>
          val newRight = Some(BinarySearchTree[A](v, None, None))
          BinarySearchTree[A](this.item, this.left, newRight)
      }
    }
  }
}