/****************************************************************************************************
 *
 * File: TypeCheck.scala
 * The type-checker for PCAT programs
 *
 ****************************************************************************************************/

package edu.uta.pcat

import scala.collection.immutable.ListMap


abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store PCAT declarations */
  var st = new SymbolTable

  /* various types */
  val anyType    = AnyType()
  val noType     = NamedType("NoType")
  val intType    = NamedType("INTEGER")
  val boolType   = NamedType("BOOLEAN")
  val floatType  = NamedType("FLOAT")
  val stringType = NamedType("STRING")

  def expandType ( tp: Type ): Type
  def typecheck ( e: Expr ): Type
  def typecheck ( e: Lvalue ): Type
  def typecheck ( e: Stmt )
  def typecheck ( e: Body, returned: Type )
  def typecheck ( e: ProcDecl )
}


class TypeCheck extends TypeChecker {

  /** the expected type of the return value from the current function */
  var expected_returned_type: Type = null

  /** If tp is a named type, expand it */
  def expandType ( tp: Type ): Type = {
    if (tp.equals(intType) || tp.equals(boolType)
        || tp.equals(floatType) || tp.equals(stringType)
        || tp.equals(anyType) || tp.equals(noType))
      tp
    else tp match {
      case NamedType(nm)
        => st.lookup(nm) match {
          case Some(TypeDec(t))
              => expandType(t)
          case _ => throw new Error("Undeclared type: "+tp)
        }
      case _ => tp
    }
  }

  /** returns true if the types tp1 and tp2 are equal under name equivalence */
  def typeEquivalence ( tp1: Type, tp2: Type ): Boolean = {
    /* AnyType() matches any RecordType(...) */
    if (tp1.equals(tp2))
      true
    else expandType(tp1) match {
      case RecordType(_)
        => tp2.equals(anyType)
      case _ => expandType(tp2) match {
        case RecordType(_)
            => tp1.equals(anyType)
        case _ => false
      }
    }
  }

  /* Tracing level */
  var level: Int = -1

  def trace[T] ( e: Any, result: => T ): T = {
    if (trace_typecheck) {
      level += 1
      println(" "*(3*level)+"** "+e)
    }
    val res = result
    if (trace_typecheck) {
      print(" "*(3*level))
      if (res == ())
        println("->")
      else println("-> "+res)
      level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  def typecheck ( e: Expr ): Type =
    trace(e,e match {
      case BinOpExp(op,l,r) => {
        val ltp = typecheck(l)
        val rtp = typecheck(r)
        if (!typeEquivalence(ltp,rtp))
          throw new Error("Incompatible types in binary operation: "+e)
        else if (op.equals("and") || op.equals("or"))
               if (typeEquivalence(ltp,boolType))
                 ltp
               else throw new Error("AND/OR operation can only be applied to booleans: "+e)
        else if (op.equals("eq") || op.equals("neq"))
               boolType
        else if (!typeEquivalence(ltp,intType) && !typeEquivalence(ltp,floatType))
               throw new Error("Binary arithmetic operations can only be applied to integer or real numbers: "+e)
        else if (op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq"))
               boolType
        else ltp
      }

      /* PUT YOUR CODE HERE */

	  case UnOpExp(op,oprnd)
	    => val tp = typecheck(oprnd)
		   if (op.equals("minus") && !typeEquivalence(tp,intType) && !typeEquivalence(tp,floatType))
			 throw new Error("Unary MINUS operation can only be applied to integer or real numbers: "+oprnd)
		   else if (op.equals("not") && !typeEquivalence(tp,boolType))
			 throw new Error("Unary NOT operation can only be applied to booleans: "+oprnd)
		   else
		     tp

	  case LvalExp(lv)
	    => typecheck(lv)
	  
	  case CallExp(n,al)
		=> st.lookup(n) match {
		     case Some(ProcDec(otp,pl,_,_,_)) 
			   => if (al.length != pl.length)
			        throw new Error("Number of parameters doesn't match number of arguments")
				  else 
				    for ((a,p) <- (al zip pl)) {
					  val tp = typecheck(a)
					  if (!typeEquivalence(tp,p._2))
					    throw new Error("The type of call arguments ("+tp+") does not match the type of the formal parameter: "+p._2)
				    }
				  otp
			 case Some(_) => throw new Error(n+" is not a procedure" )
			 case None => throw new Error("Undefined procedure: "+n)
		   }	
	  
	  case RecordExp(n,al)
	    => for ((a1,a2) <- al)
		     typecheck(a2)
		   NamedType(n)	 
  
	  case ArrayExp(n,al)
	    => for ((a1,a2) <- al) {
		     val a1_tp = typecheck(a1)
			 if (!typeEquivalence(a1_tp, intType))
			   throw new Error("First expression in array intialization must be integer: "+new ArrayExp(n,al))
			 typecheck(a2)
		   }
		   NamedType(n)
		   
	  case IntConst(n)
	    => intType
	  
	  case RealConst(n)
		=> floatType
	  
	  case StringConst(s) 
	    => stringType

      case _ => throw new Error("Wrong expression: "+e)
    } )

  /** typecheck an Lvalue AST */
  def typecheck ( e: Lvalue ): Type =
    trace(e,e match {
	  case Var("TRUE")
		=> boolType
	  case Var("FALSE")
		=> boolType
	  case Var("NIL")
		=> anyType
      case Var(name)
        => st.lookup(name) match {
              case Some(VarDec(t,_,_)) => t
              case Some(_) => throw new Error(name+" is not a variable")
              case None => throw new Error("Undefined variable: "+name)
      }

      /* PUT YOUR CODE HERE */
	  
	  case ArrayDeref(a,i)
	    => val i_tp = typecheck(i)
		   if (!typeEquivalence(i_tp,intType))
		     throw new Error("Array index must be integer: "+new ArrayDeref(a,i))
		   val a_tp = typecheck(a)
		   expandType(a_tp) match {
		     case ArrayType(s) => NamedType(s)		   
		   }
		   
	  case RecordDeref(r,a)
	    => var tp = typecheck(r)
		   expandType(tp) match {
		     case RecordType(cl) => var returnValue = noType
									var isValidAttribute = false
									for((s1,s2) <- cl) {
									  if (s1==a) {
									    returnValue = NamedType(s2)
									    isValidAttribute = true
									  }
									}
									if (isValidAttribute)
									  returnValue
									else  
									  throw new Error("Record does not have the component: "+a)
		   }
		   
      case _ => throw new Error("Wrong lvalue: "+e)
    } )

  /** typecheck the body of a function */
  def typecheck ( e: Stmt ) {
    trace(e,
          e match {
      case AssignSt(d,s)
        => if (!typeEquivalence(typecheck(d),typecheck(s)))
               throw new Error("Incompatible types in assignment: "+e)

      /* PUT YOUR CODE HERE */

	  case CallSt(n,al)
	    => st.lookup(n) match {
		     case Some(ProcDec(otp,pl,_,_,_)) 
			   => if (al.length != pl.length)
			        throw new Error("Number of parameters doesn't match number of arguments")
				  else 
				    for ((a,p) <- (al zip pl)) {
					  val tp = typecheck(a)
					  if (!typeEquivalence(tp,p._2))
					    throw new Error("The type of call arguments ("+tp+") does not match the type of the formal parameter: "+p._2)
				    }
				  val returnValue: List[String] = List.fill(al.length)(new String("()"))
			      returnValue
			 case Some(_) => throw new Error(n+" is not a procedure" )
			 case None => throw new Error("Undefined procedure: "+n)
		   }

	  case ReadSt(al)
	    => al.foreach(typecheck(_))
	  
	  case WriteSt(al)
		=> al.foreach(typecheck(_))
	  
	  case IfSt(c,s1,s2)
        => val tp = typecheck(c)
		   if (!typeEquivalence(tp,boolType))
		     throw new Error("Expected a boolean in IF test: "+c)
		   typecheck(s1)
           typecheck(s2)
		   
	  case WhileSt(c,b)
		=> val tp = typecheck(c)
		   if (!typeEquivalence(tp,boolType))
		     throw new Error("Expected a boolean in WHILE test: "+c)
		   typecheck(b)		
		
	  case LoopSt(b)
	    => typecheck(b)
		
	  case ForSt(v,init,stp,inc,b)
		=> st.begin_scope()
		   st.insert(v, VarDec(intType,0,0))
		   val i_tp = typecheck(init)
		   if (!typeEquivalence(i_tp,intType))
		     throw new Error("initial value in FOR loop must be integer: "+init)
		   val s_tp = typecheck(stp)
		   if (!typeEquivalence(i_tp,intType))
		     throw new Error("step in FOR loop must be integer: "+stp)
		   typecheck(inc)
		   typecheck(b)
		   st.end_scope()
		   
	  case ExitSt()
		=>
	  
	  case ReturnValueSt(v)
		=> val unused = typecheck(v)
		   
	  case ReturnSt()
		=>
	  
	  case SeqSt(sl)
        => sl.foreach(typecheck(_))	  
	  
      case _ => throw new Error("Wrong statement: "+e)
    } )
  }

  /** typecheck the body of a function */
  def typecheck ( e: Body, returned: Type ) {
    trace(e,
          e match {
      case Body(ds,s) => {
        ds.foreach(typecheck(_))
        expected_returned_type = returned
        s.foreach(typecheck(_))
      }
    } )
  }

  /** typecheck a declaration block */
  def typecheck ( e: Declaration ) {
    trace(e,
          e match {
      case TypeDecls(tds)
        => for ( TypeDecl(n,t) <- tds )
              st.insert(n,TypeDec(t))
      case VarDecls(vds)
        => for ( VarDecl(vs,t,u) <- vds; v <- vs )
                if (t == "NoType")
                  st.insert(v,VarDec(typecheck(u),0,0))
                else if (!typeEquivalence(typecheck(u),NamedType(t)))
                       throw new Error("Incompatible types in variable declaration: "+e)
                else st.insert(v,VarDec(NamedType(t),0,0))
      case ProcDecls(pds) => {
        for ( ProcDecl(f,ot,ps,b) <- pds )
            st.insert(f,ProcDec(NamedType(ot),
                                ps.flatMap({
                                    case (vs,t) => vs.map(_ -> NamedType(t))
                                }),"",0,0))
        for ( ProcDecl(f,ot,ps,b) <- pds ) {
          st.begin_scope()
          for ( (vs,t) <- ps; v <- vs )
              st.insert(v,VarDec(NamedType(t),0,0))
          typecheck(b,NamedType(ot))
          st.end_scope()
        }
      }
    } )
  }

  /** typecheck the main program */
  def typecheck ( e: ProcDecl ) {
    try {
      trace(e,
		    e match {
        case ProcDecl(f,ot,ps,b) => {
            st.begin_scope()
            typecheck(b,NamedType(ot))
            st.end_scope()
        }
      } )
    } catch {
        case e: Error => println("*** Type checking error: " + e)
        sys.exit(-1)
    }
  }
}
