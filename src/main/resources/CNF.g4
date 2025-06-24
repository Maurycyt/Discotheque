grammar CNF;

cnfFormulaList : cnfFormula* ;

// Turns out, that TPTP cnf formulae do not have conjunctions, so they have a single clause.
cnfFormula : 'cnf' '/' name=LowerIdent ':' wrappedCnfClause ;

wrappedCnfClause
    : '(' cnfClause ')'
    | cnfClause
    ;

cnfClause : cnfLiteral ('|' cnfLiteral)* ;

cnfLiteral
    : negated=Tilde? relation     # LNamed
    | term comp=Comparison term   # LComp
    ;

relation : name=LowerIdent '(' termList ')' ;

term
    : constant                      # TConstant
    | variable                      # TVariable
    | functor                       # TFunctor
    ;

functor : name=LowerIdent '(' termList ')' ;

constant : name=LowerIdent ;

variable : name=UpperIdent ;

termList : term (',' term)* ;

Tilde : '~' ;
Comparison : '=' | '!=' ;

LowerIdent : [a-z] [a-zA-Z0-9_]* ;
UpperIdent : [A-Z] [a-zA-Z0-9_]* ;

WS : [ \t\r\n]+ -> skip ;
COMMENT : '%' ~[\r\n]* -> skip ;
