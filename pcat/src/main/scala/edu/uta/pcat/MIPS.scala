/****************************************************************************************************
 *
 * File: MIPS.scala
 * Generation of MIPS code from IR code
 *
 ****************************************************************************************************/

package edu.uta.pcat

/** representation of a MIPS register */
case class Register ( val reg: String ) {
    override def toString (): String = reg
}


/** a pool of available registers */
class RegisterPool {

  val all_registers
        = List( "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
                "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7" )

  var available_registers = all_registers.map(Register(_))

  /** is register reg temporary? */
  def is_temporary ( reg: Register ): Boolean =
    reg match {
      case Register(n) => all_registers.contains(n)
    }

  /** return the next available temporary register */
  def get (): Register =
    available_registers match {
      case reg::rs => {
        available_registers = rs
        reg
      }
      case _ => throw new Error("*** Run out of registers")
    }

  /** recycle (put back into the register pool) the register reg (if is temporary) */
  def recycle ( reg: Register ) {
    if (available_registers.contains(reg))
      throw new Error("*** Register has already been recycled: "+reg)
    if (is_temporary(reg))
      available_registers = reg::available_registers
  }

  /** return the list of all temporary registers currently in use */
  def used (): List[Register] = {
    for ( reg <- all_registers
          if (!available_registers.contains(Register(reg))) )
      yield Register(reg)
  }
}


abstract class MipsGenerator {
  def clear ()
  def emit ( e: IRstmt )
}


class Mips extends MipsGenerator {

  var assert_count = 0

  /** emit a MIPS label */
  def mips_label ( s: String ) {
    PCAT.out.println(s + ":")
  }

  /** emit MIPS code with no operands */
  def mips ( op: String ) {
    PCAT.out.println("        " + op)
  }

  /** emit MIPS code with operands */
  def mips ( op: String, args: String ) {
    PCAT.out.print("        " + op)
    for ( i <- op.length to 10)
        PCAT.out.print(" ")
    PCAT.out.println(args)
  }

  /** a pool of temporary registers */
  var rpool = new RegisterPool

  /** clear the register pool */
  def clear () {
    rpool = new RegisterPool
  }

  var name_counter = 0

  /** generate a new  label name */
  def new_label (): String = {
    name_counter += 1
    "L_" + name_counter
  }

  /** generate MIPS code from the IR expression e and return the register that will hold the result */
  def emit ( e: IRexp ): Register = {
    e match {
      case Mem(Binop("PLUS",Reg(r),IntValue(n))) => {
        val reg = rpool.get()
        mips("lw",reg + ", " + n + "($" + r + ")")
        reg
      }
      case Binop("AND",x,y) => {
        val label = new_label()
        val left = emit(x)
        val reg = left
        mips("beq",left+", 0, "+label)
        val right = emit(y)
        mips("move",left+", "+right)
        mips_label(label)
        rpool.recycle(right)
        reg
      }
      case Call(f,sl,args) => {
        val used_regs = rpool.used()
        val size = (used_regs.length+args.length)*4
        /* allocate space for used temporary registers */
        if (size > 0)
            mips("subu","$sp, $sp, "+size)
        /* push the used temporary registers */
        var i = size
        for (r <- used_regs) {
            mips("sw",r + ", " + i + "($sp)")
            i -= 4
        }
        /* push arguments */
        i = args.length*4
        for (a <- args) {
          val reg = emit(a)
          mips("sw",reg + ", " + i + "($sp)")
          rpool.recycle(reg)
          i -= 4
        }
        /* set $v0 to be the static link */
        val sreg = emit(sl)
        mips("move","$v0, " + sreg)
        rpool.recycle(sreg)
        mips("jal",f)
        i = size
        /* pop the used temporary registers */
        for (r <- used_regs) {
            mips("lw",r + ", " + i + "($sp)")
            i -= 4
        }
        /* deallocate stack from args and used temporary registers */
        if (size > 0)
            mips("addu","$sp, $sp, "+size)
        val res = rpool.get()
        mips("move",res + ", $a0")
        /* You shouldn't just return $a0 */
        res
      }

      /* PUT YOUR CODE HERE */
	  
	  case IntValue(value) => {
	    val regValue = rpool.get()
		mips("li", regValue+", "+value)
		
		regValue		
	  }
	  
	  case Mem(addr) => {
	    val regAddr = emit(addr)		
		val regMem = rpool.get()		
		mips("lw", regMem+", ("+regAddr+")")
		
		rpool.recycle(regAddr)	
		regMem
	  }
	  
	  case Reg(name) => {
	    val reg = rpool.get()
		mips("move", reg+", $"+name)
		
		reg	  
	  }
	  
	  case Binop(op, left, right) => {
	    val regLeft = emit(left)
		val regRight = emit(right)
		
		if (op.equals("PLUS")) {
		  mips("addu", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("MINUS")) {
		  mips("subu", regLeft+", "+regLeft+", "+regRight)		
		}
		else if (op.equals("TIMES")) {
		  mips("mul", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("SLASH")) {
		  mips("div", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("DIV")) {
		  mips("div", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("MOD")) {
		  mips("rem", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("OR")) {
		  mips("or", regLeft+", "+regLeft+", "+regRight)
		}
		else if (op.equals("GT")) {
		  mips("sgt", regLeft+", "+regLeft+", "+regRight)		
		}
		else if (op.equals("LT")) {
		  mips("slt", regLeft+", "+regLeft+", "+regRight)		
		}				
		else if (op.equals("EQ")) {
		  mips("seq", regLeft+", "+regLeft+", "+regRight)		
		}
		else if (op.equals("GEQ")) {
		  mips("sge", regLeft+", "+regLeft+", "+regRight)		
		}
		else if (op.equals("LEQ")) {
		  mips("sle", regLeft+", "+regLeft+", "+regRight)	
		}
		else if (op.equals("NEQ")) {
		  mips("sne", regLeft+", "+regLeft+", "+regRight)		
		}
		
		rpool.recycle(regRight)
		regLeft
	  }
	  	  
	  case Unop(op, oprnd) => {
	    val regOprnd = emit(oprnd)
		
		if (op.equals("NOT")) {
		  mips("seq", regOprnd+", "+regOprnd+", 0")		
		} 
		else if (op.equals("MINUS")) {
		  mips("neg", regOprnd+", "+regOprnd)		
		}
		
		regOprnd
	  }
	  
	  case Allocate(size) => {
		val regSize = emit(size)
		val regLength = rpool.get()

		mips("li", regLength+", 4")
		mips("mul", regSize+", "+regSize+", "+regLength)
		mips("move", regLength+", $gp")
		mips("addu", "$gp, $gp, "+regSize)
		
		rpool.recycle(regSize)
		regLength
	  }

      case _ => throw new Error("Unknown IR: "+e)
    }
  }

  /** generate MIPS code from the IR statement e */
  def emit ( e: IRstmt ) {
    e match {
      case Move(Mem(Binop("PLUS",Reg(r),IntValue(n))),u) => {
        val src = emit(u)
        mips("sw",src + ", " + n + "($" + r + ")")
        rpool.recycle(src)
      }

      /* PUT YOUR CODE HERE */
	  
	  case Move(Mem(dest), src) => {
	    val regDest = emit(dest)
		val regSrc = emit(src)
		mips("sw", regSrc+", ("+regDest+")")	  

		rpool.recycle(regDest)
		rpool.recycle(regSrc)  
	  }
	  
	  case Move(Reg(dest), Reg(src)) => {
	    mips("move", "$"+dest+", $"+src)
	  }

	  case Move(Reg(dest), src) => {
	    val regSrc = emit(src)
		mips("move", "$"+dest+", "+regSrc)
	  }
	  
	  case Move(dest, src) => {
	    val regDest = emit(dest)
		val regSrc = emit(src)
		mips("move", regDest+", "+regSrc)	  

		rpool.recycle(regDest)
		rpool.recycle(regSrc)
	  }
	  
	  case Label(name) => {
	    if (name.equals("main")) {
		  mips(".globl", name)
		  mips(".data")
		  mips_label("AF_")
		  mips(".asciiz", "\"\\n*** Assertion Failure at address: ASSERT_\"")
		  mips_label("ENDL_")
		  mips(".asciiz", "\"\\n\"")
		  mips(".text")
		  mips_label("Assertion_failure")
		  val reg = rpool.get()
		  mips("move", reg+", $v0")
		  mips("li", "$v0, 4")
		  mips("la", "$a0, AF_")
		  mips("syscall")
		  mips("move", "$a0, "+reg)
		  mips("li", "$v0, 1")
		  mips("syscall")
		  mips("li", "$v0, 4")
		  mips("la", "$a0, ENDL_")
		  mips("syscall")
		  mips("li", "$v0, 10")
		  mips("syscall")
		  mips_label(name)
		  
		  rpool.recycle(reg)
		}
		else {
		  mips_label(name)
		}
	  }
	  
	  case Jump(name) => {
	    mips("j", name)	  
	  }
	  
	  case CJump(cond, label) => {
	    val regCond = emit(cond)
		mips("beq", regCond+", 1, "+label)
	  }
	  
	  case CallP(name, staticLink, args) => {
	    emit(Call(name, staticLink, args))
	  }
	  
	  case SystemCall(name, arg) => {
	    if (name.equals("WRITE_STRING")) {
		  arg match {
		    case StringValue(s) => {
			  if (s.equals("\\n")) {
			    mips("li", "$v0, 4")
				mips("la", "$a0, ENDL_")
			    mips("syscall")			  
			  }
			  else {
				mips(".data")
				mips(".align", "2")

				val label = new_label()
				mips_label(label)
				
				mips(".asciiz", "\""+s+"\"")
				mips(".text")
				val reg1 = rpool.get()
				mips("la", reg1+", "+label)
				mips("move", "$a0, "+reg1)
				mips("li", "$v0, 4")				
				mips("syscall")		
			  }
			}
		  }
		}
		else if (name.equals("WRITE_INT")) {
		  val regArg = emit(arg)
		  
		  mips("move", "$a0, "+regArg)
		  mips("li", "$v0, 1")		
		  mips("syscall")
		  
		  rpool.recycle(regArg)
		}		
		else if (name.equals("READ_INT")) {
		  arg match {
			case Mem(memArg) => {
			  mips("li", "$v0, 5")
			  mips("syscall")
			  val regMemArg = emit(memArg)
			  mips("sw", "$v0, ("+regMemArg+")")		

			  rpool.recycle(regMemArg)
			}
		  }		
		}
	  }

	  case Return() => {
	    mips("jr", "$ra")
	  }
	  
	  case Assert(cond) => {
	    val regCond = emit(cond)
		assert_count += 1
		mips_label("ASSERT_"+assert_count)
		mips("li", "$v0, "+assert_count)
		mips("beq", regCond+", 0, Assertion_failure")

		rpool.recycle(regCond)
	  }	
	  
      case _ => throw new Error("Unknown IR: "+e)
    }
  }
}
