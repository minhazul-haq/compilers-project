package edu.uta.pcat

/** Intermediate Representations for Expressions */
sealed abstract class IRexp
case class IntValue ( value: Int ) extends IRexp
case class StringValue ( value: String ) extends IRexp
case class RealValue ( value: Float ) extends IRexp
/** the memory content at a given address */
case class Mem ( address: IRexp ) extends IRexp
/** a register value */
case class Reg ( name: String ) extends IRexp
/** binary operations: GT, LT, EQ, GE, LE, NE, PLUS, MINUS, TIMES, SLASH, DIV, MOD, AND, OR */
case class Binop ( op: String, left: IRexp, right: IRexp ) extends IRexp
/** unary operations: MINUS, NOT */
case class Unop ( op: String, operand: IRexp ) extends IRexp
/** call a function by providing a static link and by passing arguments and return back the result */
case class Call ( name: String, static_link: IRexp, arguments: List[IRexp] ) extends IRexp
/** evaluate the statement and return the value */
case class ESeq ( stmt: IRstmt, value: IRexp ) extends IRexp
/** if the condition is true return the value, otherwise raise an exception */
case class Allocate ( size: IRexp ) extends IRexp

/** Intermediate Representations for Statements */
sealed abstract class IRstmt
/** store the source to the destination (a Mem or a Reg IRexp) */
case class Move ( destination: IRexp, source: IRexp ) extends IRstmt
/** define a label to be the current address */
case class Label ( name: String ) extends IRstmt
/** jump to a label */
case class Jump ( name: String ) extends IRstmt
/** jump to a label if condition is true */
case class CJump ( condition: IRexp, label: String ) extends IRstmt
/** evaluate a sequence of statements */
case class Seq ( stmts: List[IRstmt] ) extends IRstmt
/** call a procedure by providing a static link and by passing arguments */
case class CallP ( name: String, static_link: IRexp, arguments: List[IRexp] ) extends IRstmt
/** a system call can be: READ_INT, READ_FLOAT, WRITE_INT, WRITE_FLOAT, WRITE_BOOL, WRITE_STRING */
case class SystemCall ( name: String, arg: IRexp ) extends IRstmt
/** return from a function/procedure */
case class Return ()  extends IRstmt
/**  if the condition is false raise an exception */
case class Assert ( condition: IRexp ) extends IRstmt
