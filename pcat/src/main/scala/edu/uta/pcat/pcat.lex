/********************************************************************************
*
* File: pcat.lex
* The PCAT scanner
*
********************************************************************************/

package edu.uta.pcat;

import java_cup.runtime.Symbol;

%%
%class PcatLex
%public
%line
%column
%cup
%state comment

%{

  private Symbol symbol ( int type ) {
    return new Symbol(type, yyline, yycolumn);
  }

  private Symbol symbol ( int type, Object value ) {
    return new Symbol(type, yyline, yycolumn, value);
  }

  public void lexical_error ( String message ) {
    throw new Error("*** Lexical Error: " + message + " (line: " + yyline
                    + ", position: " + yycolumn + ")");
  }
%}

%%

"BEGIN"         { return symbol(sym.BEGIN); }
.               { lexical_error("Illegal character"); }
