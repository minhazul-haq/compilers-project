/****************************************************************************************************
 *
 * File: Normalize.scala
 * The IR code normalizer for PCAT programs
 *
 ****************************************************************************************************/

package edu.uta.pcat

class Normalize {

  /* convert the IR e into a list of IRs that do not contain any Seq or ESeq IRs */
  def normalize ( e: IRexp ): (IRexp,List[IRstmt]) = {
    e match {
      case Mem(a) => {
        val (na,s) = normalize(a)
        (Mem(na),s)
      }
      case Binop(op,l,r) => {
        val (nl,ls) = normalize(l)
        val (nr,rs) = normalize(r)
        (Binop(op,nl,nr),ls++rs)
      }
      case Unop(op,a) => {
        val (na,s) = normalize(a)
        (Unop(op,na),s)
      }
      case Call(f,l,as) => {
        val (nl,ls) = normalize(l)
        val nas = as.map(normalize(_))
        ( Call(f,nl,nas.map(_._1)),
          ls++nas.flatMap(_._2) )
      }
      case ESeq(s,a) => {
        val ss = normalize(s)
        val (na,as) = normalize(a)
        (na,ss++as)
      }
      case Allocate(a) => {
        val (na,s) = normalize(a)
        (Allocate(na),s)
      }
      case _ => (e,List())
    }
  }

  def normalize ( e: IRstmt ): List[IRstmt] = {
    e match {
      case Move(x,y) => {
        val (nx,xs) = normalize(x)
        val (ny,ys) = normalize(y)
        xs++ys:+Move(nx,ny)
      }
      case CJump(c,l) => {
        val (nc,s) = normalize(c)
        s:+CJump(nc,l)
      }
      case Seq(s)
        => s.flatMap(normalize(_))
      case CallP(f,l,as) => {
        val (nl,ls) = normalize(l)
        val nas = as.map(normalize(_))
        ls++nas.flatMap(_._2):+CallP(f,nl,nas.map(_._1))
      }
      case SystemCall(f,a) => {
        val (na,s) = normalize(a)
        s:+SystemCall(f,na)
      }
      case Assert(x) => {
        val (nx,xs) = normalize(x)
        xs:+Assert(nx)
      }
      case _ => List(e)
    }
  }

}
