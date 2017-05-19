package edu.uta.pcat

/** Abstract syntax tree for the main program */
case class Body ( decls: List[Declaration], body: List[Stmt] )

/** Abstract syntax trees for declarations */
sealed abstract class Declaration
case class TypeDecls ( decls: List[TypeDecl] ) extends Declaration
case class VarDecls ( decls: List[VarDecl] ) extends Declaration
case class ProcDecls ( decls: List[ProcDecl] ) extends Declaration

/** Abstract syntax trees for type declarations */
case class TypeDecl ( name: String, isType: Type )

/** Abstract syntax trees for variable declarations */
case class VarDecl ( names: List[String], typename: String, value: Expr )

/** Abstract syntax trees for procedure declarations */
case class ProcDecl ( name: String, outtype: String, params: List[(List[String],String)], body: Body )

/** Abstract syntax trees for types */
sealed abstract class Type
case class NamedType ( typename: String ) extends Type
case class ArrayType ( typename: String ) extends Type
case class RecordType ( components: List[(String,String)] ) extends Type
case class AnyType () extends Type

/** Abstract syntax trees for statements */
sealed abstract class Stmt
case class AssignSt ( destination: Lvalue, source: Expr ) extends Stmt
case class CallSt ( name: String, arguments: List[Expr] ) extends Stmt
case class ReadSt ( arguments: List[Lvalue] ) extends Stmt
case class WriteSt ( arguments: List[Expr] ) extends Stmt
case class IfSt ( condition: Expr, then_stmt: Stmt, else_stmt: Stmt ) extends Stmt
case class WhileSt ( condition: Expr, body: Stmt ) extends Stmt
case class LoopSt ( body: Stmt ) extends Stmt
case class ForSt ( variable: String, initial: Expr, step: Expr, increment: Expr, body: Stmt ) extends Stmt
case class ExitSt () extends Stmt
case class ReturnValueSt ( value: Expr ) extends Stmt
case class ReturnSt () extends Stmt
case class SeqSt ( stmts: List[Stmt] ) extends Stmt

/** Abstract syntax trees for expressions */
sealed abstract class Expr
case class BinOpExp ( op: String, left: Expr, right: Expr ) extends Expr
case class UnOpExp ( op: String, operand: Expr ) extends Expr
case class LvalExp ( lvalue: Lvalue ) extends Expr
case class CallExp ( name: String, arguments: List[Expr] ) extends Expr
case class RecordExp ( name: String, arguments: List[(String,Expr)] ) extends Expr
case class ArrayExp ( name: String, arguments: List[(Expr,Expr)] ) extends Expr
case class IntConst ( value: Int ) extends Expr
case class RealConst ( value: Float ) extends Expr
case class StringConst ( value: String ) extends Expr

/** Abstract syntax trees for lvalues */
sealed abstract class Lvalue
case class Var ( name: String ) extends Lvalue
case class ArrayDeref ( array: Lvalue, index: Expr ) extends Lvalue
case class RecordDeref ( record: Lvalue, attribute: String ) extends Lvalue
