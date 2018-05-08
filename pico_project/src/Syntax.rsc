module Syntax

import Prelude;

lexical Id  = ([a-z][a-z0-9]* !>> [a-z0-9]) \ PicoKeywords;
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";

keyword PicoKeywords = "begin" | "end" | 
                       "declare" | 
                       "if" | "then" | "else" | "fi" | 
                       "while" | "do" | "od" | 
                       "for" |                         // +kw for statement
                       ;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"
   | @category="Comment" "%%" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

syntax Type 
   = natural:"natural" 
   | string :"string" 
   | boolean : "boolean" // new typedef for boolean variable 
   ;

syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   | forStat: "for" Id index ":="  Expression val ", " Expression cond ", " Expression strdUpd "do" {Statement ";"}* body "od" // syntax definition for statement
  ;  
     
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | boolCon: Boolean boolcon          // +boolean expression
   | bracket "(" Expression e ")"
   > left conc: Expression lhs "||" Expression rhs
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
   > right ( not: "not" Expression rhs) 						// + logic not expression higher prio
   > left ( equal: Expression lhs "==" Expression rhs        // + equal expression
   		  | not_equal: Expression lhs "!=" Expression rhs    // + not equal expression
   		  | and: Expression lhs "and" Expression rhs         // +logic and expression prio
   		  | or: Expression lhs "or" Expression rhs           // +logic or expression prio
   		  )
  ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}