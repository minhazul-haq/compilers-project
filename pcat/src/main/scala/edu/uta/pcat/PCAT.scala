package edu.uta.pcat

import java.io.FileReader
import java.io.PrintStream
import java_cup.runtime._

object PCAT {

  val use_project_1_solution = false
  val use_project_2_and_3_solution = false
  val use_project_4_solution = false
  val use_project_5_solution = false

  var out: PrintStream = null
  var program_ast: ProcDecl = null

  def setAST ( ast: ProcDecl ) {
    program_ast = ast
  }

  def main ( args: Array[String] ) {
    if (args.length < 2 || !List("1","2","3","4","5","6").contains(args(0)))
      throw new Error("First parameter must be the phase number [1-6]")
    var phase = args(0).toInt
    for ( file <- args.tail ) {
      println("********* file: "+file)
      val scanner: Scanner = if (use_project_1_solution)
                               new PcatLex_(new FileReader(file))
                             else new PcatLex(new FileReader(file))
      if (phase == 1) {
        var t: Symbol = null
        do {
          t = scanner.next_token()
          if (t != null && t.sym > 0)
            println(PcatParser_.print(t))
        } while (t != null && t.sym > 0)
      } else {
        val parser: lr_parser = if (use_project_2_and_3_solution)
                                  new PcatParser_(scanner)
                                else new PcatParser(scanner)
        parser.parse
        val prettyprinter = new PrettyPrint(80,"")
        if (phase == 3)
          println(prettyprinter.pretty(program_ast.toString()))
        if (phase > 3) {
          val typechecker: TypeChecker = if (use_project_4_solution)
                                           new TypeCheck_
                                         else new TypeCheck
          if (phase == 4)
            typechecker.trace_typecheck = true
          typechecker.typecheck(program_ast)
          if (phase > 4) {
              val codegenerator: CodeGenerator = if (use_project_5_solution)
                                                   new Code_(typechecker)
                                                 else new Code(typechecker)
              val ir = codegenerator.code(program_ast)
              if (phase == 5)
                println(prettyprinter.pretty(ir.toString()))
              val normalizer = new Normalize
              val nir = normalizer.normalize(ir)
              if (phase == 6) {
                val mipsgenerator: MipsGenerator = new Mips
                out = new PrintStream(file.dropRight(5)+".s")
                for ( s <- nir ) {
                  mipsgenerator.clear
                  val prettyprinter = new PrettyPrint(80,"# ")
                  out.println("#\n"+prettyprinter.pretty(s.toString())+"\n#")
                  mipsgenerator.emit(s)
                }
              }
            }
        }
      }
    }
  }
}
