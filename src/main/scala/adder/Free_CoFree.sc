

import adder.Adder.AdderT
import adder.Limit
import adder.Transformer._

import scalaz._
import Scalaz._

val inter: AdderT[ReaderT[StateT[Id,Count, ?], Limit, ?], Count] => Id[Count] = interpret[Id,Count](7,5) _


//
val flim: AdderT[ReaderT[StateT[Id, Count, ?], Limit, ?], Count] =
     Limit.findLimit[ReaderT[StateT[Id, Count, ?], Limit, ?]]()

inter(flim)

