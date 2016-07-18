package adder


import scalaz._
import Scalaz._

//taken from http://dlaing.org/cofun/posts/free_and_cofree.html


object Adder {
  //here we use the non coyoneda Free setup, so as to stick closer to the examples
  //todo: turn this into AdderF[Result,Next] so that it matches more closely the pattern when using Coyoneda and simple DLSs
  //Adder Functor
  sealed trait AdderF[+Next]

  //add an Int to the total, after which we get hold of a Bool which indicates if we can continue
  case class Add[Next](i: Int, f: Boolean=>Next) extends AdderF[Next]

  //clear the total, and then move on to the next DSL action.
  case class Clear[Next](next: Next) extends AdderF[Next]

  //ask for the total
  case class Total[Next](f: Int=>Next) extends AdderF[Next]

  implicit def dslFunctor: Functor[AdderF] = new Functor[AdderF] {
    override def map[A, B](fa: AdderF[A])(g: A => B): AdderF[B] = fa match {
      case Add(i,f) => Add[B](i,(b: Boolean)=> g(f(b))) //detailed
      case Clear(next) => Clear(g(next))
      case Total(f) => Total(f andThen g)  //simpler
    }
  }

  type Adder[A] = Free[AdderF,A]

  def add(i: Int): Adder[Boolean] = Free.liftF(Add[Boolean](i, identity))

  def clear(): Adder[Unit] = Free.liftF(Clear(()))

  def total(): Adder[Int] = Free.liftF(Total[Int](identity))

  type AdderT[M[_],A] = FreeT[AdderF,M,A]

  def addT[M[_]: Applicative](i: Int): AdderT[M,Boolean] = FreeT.liftF[AdderF,M,Boolean](Add[Boolean](i,identity))

  def clearT[M[_]: Applicative](): AdderT[M,Unit] = FreeT.liftF[AdderF,M,Unit](Clear(()))

  def totalT[M[_]: Applicative](): AdderT[M,Int] = FreeT.liftF[AdderF,M,Int](Total[Int](identity))


}
