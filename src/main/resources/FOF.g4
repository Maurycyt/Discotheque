grammar FOF;

fofFormulaList : fofFormula* ;

fofFormula : 'fof' '/' name=LowerIdent ':' formula ;

formula
    : '(' formula ')'                                       # FWrapped
    | literal                                               # FLiteral
    | Tilde formula                                         # FNegated
    | formula BinaryOp formula                              # FBinary
    | quantifier=Quantifier '[' variables ']' ':' formula   # FQuantified
    ;

literal
    : relation                    # LNamed
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
variables : variable (',' variable)* ;

termList : (term (',' term)*)? ;

Tilde : '~' ;
Comparison : '=' | '!=' ;
Quantifier : '!' | '?' ;
BinaryOp : '&' | '|' | '=>' | '<=' | '<=>';

LowerIdent : [a-z] [a-zA-Z0-9_]* ;
UpperIdent : [A-Z] [a-zA-Z0-9_]* ;

WS : [ \t\r\n]+ -> skip ;
COMMENT : '%' ~[\r\n]* -> skip ;
