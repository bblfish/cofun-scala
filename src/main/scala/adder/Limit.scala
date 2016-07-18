package adder

import scalaz._
import Scalaz._

/**
  * Created by hjs on 17/06/2016.
  */
object Limit {
  import adder.Adder._
  import adder.Transformer.{Limit, Count}


  def findLimit[M[_]: Monad](): AdderT[M,Int] = {
    for {
      t <- totalT[M]()
      _ <- clearT[M]()
      r <- findLimit2[M].exec(0)
      _ <- clearT[M]()
      _ <- addT[M](t)
    } yield r
  }

  protected
  def findLimit2[M[_]: Monad]: StateT[AdderT[M,?],Limit,Boolean] = {
    import StateT._
    type StateTLim[M[_],A] = StateT[M,Limit,A]

    val hoist = StateT.StateMonadTrans[Limit]
    val at: AdderT[M, Boolean] = addT[M](1)
    val st: StateTLim[AdderT[M,?],Boolean] = hoist.liftM[AdderT[M,?],Boolean](at)

    st.flatMap { r: Boolean =>
      if (r) {
        for {
          _ <- StateT.stateTMonadState[Limit,AdderT[M,?]].modify(n => n + 1)
          x <- findLimit2[M]
        } yield x

      }
      else StateT.stateTMonadState[Limit,AdderT[M,?]].point(r)
    }
  }
}
