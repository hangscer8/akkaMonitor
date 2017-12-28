package nathan.tree

trait Tree[+A] {
  //本树中的任何节点的值不能重复!!!(在delete时，会全部删除)
  self =>
  def isEmpty: Boolean = self == EmptyTree

  def nonEmpty = !isEmpty

  def value: A = valueOption match {
    case None => throw new Exception("空节点，无数据")
    case some: Some[A] => some.value
  }

  def toList: List[A] = {
    def loop[T](tree: Tree[T])(list: List[T]): List[T] = {
      tree match {
        case EmptyTree => list
        case Leaf(v) => v :: list
        case Branch(v, bs) => bs.flatMap(t => loop(t)(Nil)) ++ (v :: list)
      }
    }

    loop(self)(Nil)
  }

  def valueOption: Option[A] = self match {
    case EmptyTree => None
    case Leaf(v) => Some(v)
    case Branch(v, _) => Some(v)
  }

  def map[B](f: A => B): Tree[B] = self match {
    case Branch(v, bs) => Branch(f(v), bs.map(t => t.map(f)))
    case EmptyTree => EmptyTree
    case Leaf(v) => Leaf(f(v))
  }

  def foreach[B](f: A => B): Unit = self match {
    case EmptyTree => EmptyTree
    case Leaf(v) => f(v)
    case Branch(v, bs) =>
      f(v)
      bs.foreach(t => t.foreach(f))
  }

  def find(p: A => Boolean): Tree[A] = self match {
    case EmptyTree => EmptyTree //空节点
    case Leaf(v) if p(v) => self
    case Leaf(_) => EmptyTree //不满足 不要
    case Branch(v, _) if p(v) => self //找到了
    case Branch(_, bs) =>
      bs.map(t => t.find(p)).filter(t => t.nonEmpty).headOption match {
        case None => EmptyTree
        case Some(t) => t
      }
  }

  def insertChild[B >: A](p: B => Boolean)(value: B): Tree[B] = {
    Tree.insertChild(self, p, value)
  }

  def deleteSubTree(p: A => Boolean): Tree[A] = self match {
    //把满足的条件的节点以及其子节点全部删除
    case EmptyTree => EmptyTree
    case Leaf(v) => p(v) match {
      case true => EmptyTree //满足条件 删除
      case false => self //不删
    }
    case Branch(v, bs) => p(v) match {
      case true => EmptyTree //满足条件 全部删除 删除其子树
      case false => Branch(v, bs.map(t => t.deleteSubTree(p)).filter(t => t.nonEmpty)) //没找到 进入下一层递归
    }
  }

  def deleteNode(p: A => Boolean): Tree[A] = self match {
    case EmptyTree => EmptyTree
    case Leaf(v) => p(v) match {
      case true => EmptyTree
      case false => self
    }
    case Branch(v, bs) => p(v) match {
      case true =>
        //满足条件应当删除 用其第一个子节点代替(验证是否存在子节点)
        bs.filter(_.nonEmpty).headOption match {
          case some: Some[Tree[A]] =>
            Branch(some.value.value, bs.map(t => t.deleteNode(_ == some.value.value)))
          case None =>
            EmptyTree
        }
      case false =>
        Branch(v, bs.map(t => t.deleteNode(p))) //找不到，进入下一层递归
    }
  }

  def insert[B >: A](value: B): Tree[B] = self match {
    //没有平衡操作，而且还是是不可变结构
    case EmptyTree => Leaf(value)
    case Leaf(v) => Branch(v, List(Leaf(value)))
    case Branch(v, bs) => Branch(v, Leaf(value) :: bs)
  }

  def print(): Unit = {
    val gap = "    "

    def loop[B](tree: Tree[B])(depth: Int): Unit = {
      tree match {
        case Leaf(v) =>
          println(s"${gap * depth}$v")
        case Branch(v, bs) =>
          println(s"${gap * depth}$v")
          bs.foreach(t => loop(t)(depth + 1))
        case EmptyTree =>
          println(s"${gap * depth}E")
      }
    }

    loop(self)(0)
  }
}

case object EmptyTree extends Tree[Nothing]

//空节点

case class Leaf[+A](v: A) extends Tree[A]

//叶节点

class Branch[+A](val v: A, val bs: List[Tree[A]]) extends Tree[A] {
  override def equals(obj: scala.Any): Boolean = {
    obj.isInstanceOf[Branch[_]] match {
      case false => //不是分支节点
        obj.isInstanceOf[Leaf[_]] match {
          //是叶子节点吗
          case false => false
          case true =>
            val temp = obj.asInstanceOf[Leaf[_]]
            this.bs.filter(_.nonEmpty) match {
              //本节点是其他分支为空吗
              case Nil => this.v == temp.v
              case _ => false
            }
        }
      case true =>
        val temp = obj.asInstanceOf[Branch[_]]
        (temp.v == this.v) && (temp.bs == this.bs)
    }
  }
}

object Branch {
  def apply[A](v: A, bs: List[Tree[A]]): Tree[A] = bs.filter(t => t.nonEmpty) match {
    case Nil => Leaf(v)
    case list@_ => new Branch[A](v, list) //过滤掉空节点
  }

  def unapply[A](arg: Branch[A]): Option[(A, List[Tree[A]])] = Some((arg.v, arg.bs.filter(_.nonEmpty)))
}

object Tree {
  def empty[A]: Tree[A] = EmptyTree

  private[tree] def insertChild[B >: A, A](root: Tree[A], p: B => Boolean, value: B): Tree[B] = root match {
    //满足条件的节点插入子节点 不可变结构
    case EmptyTree => Leaf(value) //空节点
    case Leaf(v) if p(v) => Branch(v, List(Leaf(value))) // 此时tree是一个叶子节点 在tree节点插入子节点
    case Leaf(_) => root //fix bug ok!!
    case Branch(v, bs) if p(v) =>
      bs.map(_.valueOption).flatten.contains(value) match {
        case true =>
          root //存在满足的父节点，但是直接子节点已经存在，不插入
        case false =>
          Branch(v, Leaf(value) :: bs) //找到了 加入子节点
      }
    case Branch(v, bs) => Branch(v, bs.map(t => insertChild(t, p, value))) //没找到，递归进入下一层
  }
}