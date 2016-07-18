package adder


object Transformer {
  import scalaz._
  import Scalaz._
  import adder.Adder._

  import scalaz.syntax.KleisliFAOps


  //def findLimit[M: Monad](): AdderT[M,Int] = for {
  //  t <- totalT()
  //  _ <- clearT()
  ////  r <- findLimit2.exec(0)
  ////  _ <- clearT()
  ////  _ <- addT(t)
  //} yield r


  type Limit = Int
  type Count = Int
  type StateTC[M[_], A] = StateT[M,Count,A]
  type Base[M[_],A] = ReaderT[StateTC[M, ?], Limit, A]


  def interpret[M[_]: Monad: BindRec,A](
    limit: Limit, count: Count): AdderT[Base[M,?],A] => M[A] = {

      (interpret2[M,A] _) andThen[M[A]] runBase[M,A](limit,count)

    // the above point free notation should be equivalent to this:
    // (AdderT[Base[M,?],A]) => {
    //    val g: Base[M,A]=> M[A] = runBase[M,A](limit,count)
    //    val base: Base[M, A] = interpret2(a)
    //    g(base)
    // }
  }

  protected
  def interpret2[M[_]: Monad: BindRec,R](a: AdderT[Base[M,?],R]): Base[M,R] = {
    import StateT._
    import scalaz.syntax.kleisli._
    implicit val x = Kleisli.kleisliBindRec[StateT[M,  Count, ?],R]
    a.foldMap(new (AdderF ~> Base[M, ?]) {

      override def apply[A](mr: AdderF[A]): Base[M, A] = mr match {
        case Add(x, nextF) => {
          for {
            limit <- Kleisli.kleisliMonadReader[StateTC[M, ?], Limit].ask //Kleisli.ask
            count <- stateTMonadState[Count, M].get.liftReaderT[Limit]
            count2 = x + count
            test = count2 <= limit
            next = if (test) {
              count2
            } else count
            _ <- stateTMonadState[Count, M].put(next).liftReaderT[Limit]
          } yield {
            nextF(test)
          }
        }
        case Clear(next) => {
          val sts: StateTC[M, Unit] = stateTMonadState[Count, M].put(0)
          val x = new KleisliFAOps[StateTC[M, ?], Unit](sts)
          val res: ReaderT[StateTC[M, ?], Limit, Unit] = x.liftReaderT[Limit]
          res.map(_ => next)
        }
        case Total(nextF) => {
          val sts: StateTC[M, Count] = stateTMonadState[Count, M].get
          val x = new KleisliFAOps[StateTC[M, ?], Count](sts)
          val res: ReaderT[StateTC[M, ?], Limit, Count] = x.liftReaderT[Limit]
          for {
            count <- res
          } yield {
            nextF(count)
          }
        }
      }
    })
  }


  //runBase :: Monad m => Limit -> Count -> Base m r -> m r
  //  runBase limit count =
  //  flip evalStateT count .  |: StateT s m a  -> m a
  //    flip runReaderT limit  |: ReaderT r m a -> m a
  //where
  // evalStateT :: Monad m => StateT s m a -> s -> m a
  // runReaderT :: ReaderT r m a -> r -> m a
  protected
  def runBase[M[_]: Monad,A](
    limit: Limit, count: Count): Base[M,A]=> M[A] =
    (base: Base[M,A]) => {
      val st: StateTC[M, A] = base.run(limit)
      st.eval(count)
      //the types align, but I am not sure if it has the same meaning.
    }
}
