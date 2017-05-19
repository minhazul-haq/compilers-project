/****************************************************************************************************
 *
 * File: Code.scala
 * The IR code generator for PCAT programs
 *
 ****************************************************************************************************/

package edu.uta.pcat

import scala.collection.mutable.Stack

abstract class CodeGenerator ( tc: TypeChecker )  {
  def typechecker = tc
  def st = tc.st
  def code ( e: ProcDecl ): IRstmt
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp
}


class Code ( tc: TypeChecker ) extends CodeGenerator(tc) {

  /** holds the exit labels of loops (needed for the exit() statements) */
  var labels = new Stack[String]

  var name_counter = 0

  /** generate a new variable name */
  def new_name ( name: String ): String = {
    name_counter += 1
    name + "_" + name_counter
  }

  /** allocate a new variable at the end of the current frame and return the access code */
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp = {
    st.lookup(fname) match {
      case Some(ProcDec(rtp,params,label,level,min_offset)) => {
          // allocate variable at the next available offset in frame
          st.insert(name,VarDec(var_type,level,min_offset))
          // the next available offset in frame is 4 bytes below
          st.replace(fname,ProcDec(rtp,params,label,level,min_offset-4))
          // return the code that accesses the variable
          Mem(Binop("PLUS",Reg("fp"),IntValue(min_offset)))
      }
      case _ => throw new Error("No current function: " + fname)
    }
  }

  /** access a frame-allocated variable from the run-time stack */
  def access_variable ( name: String, level: Int ): IRexp = {
    st.lookup(name) match {
      case Some(VarDec(_,var_level,offset)) => {
        var res: IRexp = Reg("fp")
        // non-local variable: follow the static link (level-var_level) times
        for ( i <- var_level+1 to level )
            res = Mem(Binop("PLUS",res,IntValue(-8)))
        Mem(Binop("PLUS",res,IntValue(offset)))
      }
      case _ => throw new Error("Undefined variable: " + name)
    }
  }

  /** return the IR code from the Expr e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Expr, level: Int, fname: String ): IRexp = {
    e match {
      case BinOpExp(op,left,right) => {
          val cl = code(left,level,fname)
          val cr = code(right,level,fname)
          val nop = op.toUpperCase()
          Binop(nop,cl,cr)
      }
      case ArrayExp(nm,inits) => {
        // A is the array address
        val A = allocate_variable(new_name("A"),NamedType(nm),fname)
        // I is offset of end-of-array
        val I = allocate_variable(new_name("I"),typechecker.intType,fname)
        // for iterating through loops
        val i = allocate_variable(new_name("i"),typechecker.intType,fname)
        // IR that calculates the array length
        var len: IRexp = IntValue(0)
        var cs: List[IRstmt] = List()
        for ( (n,v) <- inits )
          if (n == IntConst(1)) {   // don't need a loop for this
              val cv = code(v,level,fname)
              len = Binop("PLUS",len,IntValue(1))
              cs = cs :+ Seq(List(Move(Mem(Binop("PLUS",A,I)),cv),
                                  Move(I,Binop("PLUS",I,IntValue(4)))))
          } else {
            val cn = code(n,level,fname)
            val cv = code(v,level,fname)
            val loop = new_name("loop")
            val exit = new_name("exit")
            len = Binop("PLUS",len,cn)
            cs = cs :+ Seq(List(Move(i,IntValue(0)),
                                Label(loop),          // a for-loop
                                CJump(Binop("GEQ",i,cn),exit),
                                Move(Mem(Binop("PLUS",A,I)),cv),
                                Move(I,Binop("PLUS",I,IntValue(4))),
                                Move(i,Binop("PLUS",i,IntValue(1))),
                                Jump(loop),
                                Label(exit)))
          }
        ESeq(Seq(List(Move(A,Allocate(Binop("PLUS",len,IntValue(1)))),    // allocate len+1 words for A
                      Move(Mem(A),len),         // set the array length
                      Move(I,IntValue(4)))      // first available offset is 4
                 ++ cs),
             A)          
      }

      /* PUT YOUR CODE HERE */

    case UnOpExp(op,oprnd) => {
      val nop = op.toUpperCase()
      val cOprnd = code(oprnd,level,fname)
      Unop(nop,cOprnd)    
    }
    
    case LvalExp(lv) => {
      code(lv,level,fname)
    }

    case CallExp(n,al) => {
      var alist: List[IRexp] = List()

    for( a <- al ) {
      alist = alist :+ code(a,level,fname)    
    }
        
    var procLabel = n
    var procLevel = 0
    
    st.lookup(n) match {
      case Some(ProcDec(_,_,label,level,_)) => {
        val lastIndexOfUnderscore = label.lastIndexOf('_')
      val n2 = label.substring(0,lastIndexOfUnderscore)
        if ( n.equals(n2) ) {
        procLabel = label
        procLevel = level 
      }
      } 
    } 
              
    if (level - procLevel == -1) {
      Call(procLabel,Reg("fp"),alist)
    }
    else {
      var staticLink = Mem(Binop("PLUS",Reg("fp"),IntValue(-8)))
      
      for (i <- 1 to (level - procLevel))
        staticLink = Mem(Binop("PLUS",staticLink,IntValue(-8)))
  
      Call(procLabel,staticLink,alist)
    }
    }
    
      case RecordExp(n,al) => {
    val R = allocate_variable(new_name("R"),NamedType(n),fname)
    var I = 0
    var len = 0
        var cs: List[IRstmt] = List()
  
    for( (s,e) <- al ) {
      val cv = code(e,level,fname)
      cs = cs :+ Move(Mem(Binop("PLUS",R,IntValue(I))),cv)      
      len = len + 1
      I = I + 4
    }
    
    ESeq( Seq( List(Move(R,Allocate( IntValue(len) ))) ++ cs), R)   
    }  

    case IntConst(v) => {
      IntValue(v)
    }
        
    case RealConst(v) => {
      RealValue(v)
    }
        
    case StringConst(v) => {
      StringValue(v)
    }
        
      case _ => throw new Error("Wrong expression: "+e)
    }
  }

  /** return the IR code from the Statement e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Stmt, level: Int, fname: String ): IRstmt = {
    e match {
      case AssignSt(v,u) => {
            val cd = code(v,level,fname)
            val cs = code(u,level,fname)
            Move(cd,cs)
      }

      /* PUT YOUR CODE HERE */

    case CallSt(n,al) => {
      var alist: List[IRexp] = List()
  
    for( a <- al ) {
      alist = alist :+ code(a,level,fname)    
    }
        
    var procLabel = n
    var procLevel = 0
    
    st.lookup(n) match {
      case Some(ProcDec(_,_,label,level,_)) =>
        val lastIndexOfUnderscore = label.lastIndexOf('_')
      val n2 = label.substring(0,lastIndexOfUnderscore)
        if ( n.equals(n2) ) {
        procLabel = label
        procLevel = level 
      }
    } 
              
    if (level - procLevel == -1) {
      CallP(procLabel,Reg("fp"),alist)
    }
    else {
      var staticLink = Mem(Binop("PLUS",Reg("fp"),IntValue(-8)))
      
      for (i <- 1 to (level - procLevel))
        staticLink = Mem(Binop("PLUS",staticLink,IntValue(-8)))
  
      CallP(procLabel,staticLink,alist)
    }
    }
        
    case ReadSt(lvl) => {
    var list: List[IRstmt] = List()
    for (lv <- lvl) {
      val tp = typechecker.typecheck(lv)
      if (tp==typechecker.intType)
      list = list :+ SystemCall("READ_INT", code(lv,level,fname)) 
      if (tp==typechecker.floatType)
        list = list :+ SystemCall("READ_FLOAT", code(lv,level,fname)) 
    } 
    Seq(list)     
    }
        
    case WriteSt(al) => {
    var list: List[IRstmt] = List()
    for (a <- al) {
      a match {
        case IntConst(n) =>
        list = list :+ SystemCall("WRITE_INT", IntValue(n))
      case RealConst(f) => 
        list = list :+ SystemCall("WRITE_FLOAT", RealValue(f))
      case StringConst(s) => 
        list = list :+ SystemCall("WRITE_STRING", StringValue(s))
      case _ => {
        val tp = typechecker.typecheck(a)
        if (tp==typechecker.intType)
          list = list :+ SystemCall("WRITE_INT", code(a,level,fname)) 
        else if (tp==typechecker.floatType)
          list = list :+ SystemCall("WRITE_FLOAT", code(a,level,fname)) 
            else if (tp==typechecker.boolType)
          list = list :+ SystemCall("WRITE_BOOL", code(a,level,fname))  
        else if (tp==typechecker.stringType)
          list = list :+ SystemCall("WRITE_STRING", code(a,level,fname))      
      }
      }
    } 
    list = list :+ SystemCall("WRITE_STRING",StringValue("\\n"))
    Seq(list)     
    }   

    case IfSt(cond,s1,s2) => {
      val cont = new_name("cont")
    val exit = new_name("exit")
    val cS1 = code(s1,level,fname)
    val cS2 = code(s2,level,fname)
      
    Seq(List( CJump( code(cond,level,fname), exit ),
          cS2,
          Jump(cont),
          Label(exit),
          cS1,
                  Label(cont) ))
    }   
    
    case WhileSt(cond,b) => {
      val loop = new_name("loop")
    val exit = new_name("exit")
    
    Seq(List( Label(loop),
          CJump(Unop("NOT",code(cond,level,fname)),exit),
          code(b,level,fname),
          Jump(loop),
          Label(exit) ))
    }

    case LoopSt(b) => {
      val loop = new_name("loop")
    val exit = new_name("exit")
    labels.push(exit)
    
    Seq(List( Label(loop),
          code(b,level,fname),
          Jump(loop),
          Label(exit) ))
    }
    
    case ForSt(v,init,step,inc,b) => {
      val loop = new_name("loop")
    val exit = new_name("exit")
    
      var forVariable = allocate_variable(v,NamedType("INTEGER"),fname) 
    
    Seq(List( Move(Mem(forVariable),code(init,level,fname)),
          Label(loop),
          CJump(Binop("GT",forVariable,code(step,level,fname)),exit),
          code(b,level,fname),
          Move(Mem(forVariable),Binop("PLUS",forVariable,IntValue(1))),
          Jump(loop),
          Label(exit) ))
    } 
    
    case ExitSt() => {
      val exitLabel = labels.pop()
    Jump(exitLabel)
    }   
    
    case ReturnValueSt(v) => {
      Seq(List( Move(Reg("a0"),code(v,level,fname)),
          Move(Reg("ra"),Mem(Binop("PLUS",Reg("fp"),IntValue(-4)))),
          Move(Reg("sp"),Reg("fp")),
          Move(Reg("fp"),Mem(Reg("fp"))),
          Return() ))
    }   

    case ReturnSt() => {
      Seq(List( Move(Reg("ra"),Mem(Binop("PLUS",Reg("fp"),IntValue(-4)))),
          Move(Reg("sp"),Reg("fp")),
          Move(Reg("fp"),Mem(Reg("fp"))),
          Return() ))
    }   
        
    case SeqSt(sl) => {
      var list: List[IRstmt] = List()
    
      for ( s <- sl ) {
      list = list :+ code(s,level,fname)
    }
    
    Seq(list)
    }
    
      case _ => throw new Error("Wrong statement: " + e)
    }
  }

  /** return the IR code from the Lvalue e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Lvalue, level: Int, fname: String ): IRexp = {
    e match {
    case Var("TRUE") => IntValue(1)
    case Var("FALSE") => IntValue(0)
    case Var("NIL") => IntValue(0)
      case Var(s) => access_variable(s,level)

      /* PUT YOUR CODE HERE */

    case ArrayDeref(array,index) => {
    ESeq( Assert( Binop( "AND", 
               Binop("GEQ",code(index,level,fname),IntValue(0)),
               Binop("LT",code(index,level,fname),Mem(code(array,level,fname))) ) ) ,
        Mem( Binop( "PLUS",
              code(array,level,fname),
              Binop("TIMES",Binop("PLUS",code(index,level,fname),IntValue(1)),IntValue(4)) ) ) )
    }

    case RecordDeref(record,attr) => {
      val tp = typechecker.typecheck(record)
    var attrOffset = 0
    var foundAttr = 0
    typechecker.expandType(tp) match {
      case RecordType(cl) => for((s1,s2) <- cl) {
                   if (s1==attr)
                     foundAttr = 1
                   
                   if (foundAttr==0)
                     attrOffset += 4                   
                 }
    }
    
      ESeq( Assert(Binop("NEQ",
               code(record,level,fname),
               IntValue(0))) , 
        Mem(Binop("PLUS",
              code(record,level,fname),
              IntValue(attrOffset))) ) 
    }
    
      case _ => throw new Error("Wrong statement: " + e)
    }
  }

  /** return the IR code from the function body (level is the current function nesting level,
   *  f is the name of the current function/procedure) */
  def code ( e: Body, level: Int, f: String ): IRstmt = {
    e match {
      case Body(ds,s) => {
        val defs = Seq(ds.map(code(_,f,level)))
        val body = Seq(s.map(code(_,level,f)))
        val inits = Seq(for ( VarDecls(vds) <- ds;
                              VarDecl(vs,_,u) <- vds;
                              uc = code(u,level,f);
                              v <- vs )
                            yield Move(access_variable(v,level),uc))
        st.lookup(f) match {
          case Some(ProcDec(_,_,fname,_,offset))
            => Seq(List(defs,
                        Label(fname),
                        Move(Mem(Reg("sp")),Reg("fp")),
                        Move(Reg("fp"),Reg("sp")),
                        Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-4))),Reg("ra")),
                        Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-8))),Reg("v0")),
                        Move(Reg("sp"),Binop("PLUS",Reg("sp"),IntValue(offset))),
                        inits,
                        body,
                        Move(Reg("ra"),Mem(Binop("PLUS",Reg("fp"),IntValue(-4)))),
                        Move(Reg("sp"),Reg("fp")),
                        Move(Reg("fp"),Mem(Reg("fp"))),
                        Return()))
          case _ => throw new Error("Unkown function: "+f)
        }
      }
    }
  }

  /** return the IR code for the declaration block of function fname
   * (level is the current function nesting level) */
  def code ( e: Declaration, fname: String, level: Int ): IRstmt = {
    e match {
      case TypeDecls(tds) => {
        for ( TypeDecl(n,t) <- tds )
              st.insert(n,TypeDec(t))
        Seq(List())
      }
      case VarDecls(vds) => {
        for ( VarDecl(vs,t,u) <- vds; v <- vs )
              if (t == "NoType")
                allocate_variable(v,typechecker.typecheck(u),fname)
              else allocate_variable(v,NamedType(t),fname)
        Seq(List())
      }
      case ProcDecls(pds) => {
        for ( ProcDecl(f,ot,ps,b) <- pds )
            st.insert(f,ProcDec(NamedType(ot),
                                ps.flatMap({
                                    case (vs,t) => vs.map(_ -> NamedType(t))
                                }),new_name(f),level+1,-12))
        Seq( for ( ProcDecl(f,ot,ps,b) <- pds ) yield {
                var i = 4
                st.begin_scope()
                for ( (vs,t) <- ps.reverse; v <- vs.reverse ) {
                      st.insert(v,VarDec(NamedType(t),level+1,i))
                      i += 4
                }
                val res = code(b,level+1,f)
                st.end_scope()
                res
            } )
      }
    }
  }

  /** generate code for the main program */
  def code ( e: ProcDecl ): IRstmt = {
    e match {
      case ProcDecl(f,ot,ps,b) => {
          var i = 4
          st.begin_scope()
          st.insert(f,ProcDec(NamedType(ot),List(),f,1,-12))
          val res = code(b,1,f)
          st.end_scope()
          res
      }
    }
  }

}
