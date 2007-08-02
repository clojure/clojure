/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/
 
grammar Reader;
options { language=Java; backtrack=true; memoize=true;}
@header { 
package clojure.lang; 
import java.math.BigInteger;
import java.io.Writer;
import java.io.PrintWriter;
} 
@lexer::header { 
/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/
 package clojure.lang; 
} 
@lexer::members{
	RecognitionException rex = null;
	
	public void recover(RecognitionException re) {
		super.recover(re);
		if(rex == null)
			rex = re;	
	}
	
	public void reportError(RecognitionException e) {
		if(rex == null)
			rex = e;
	}
}

@members{
//add these lines to expressions, dotExpression, args
// in the e=expression(); case
//after:
//            	    if (failed) return val;

    /*
    //WARNING- HAND MODIFIED!
    if(es == null) es = new ArrayList();
    es.add(e);
    // END HAND MODIFIED
    */	


//add this to mapexpr:
    /*
    //WARNING- HAND MODIFIED!
    	                val = val.assoc(k, v);
    // END HAND MODIFIED
    */

final static Symbol DOTDOT = Symbol.create(null,"..");
final static Symbol QUOTE = Symbol.create(null,"quote");
final static Symbol META = Symbol.create(null,"meta");

public static void main(String[] args) throws Exception { 
Writer w = new PrintWriter(System.out);
// Create an input character stream from standard in
CharStream input = new ANTLRFileStream(args[0]);
// Create an ExprLexer that feeds from that stream
ReaderLexer lexer = new ReaderLexer(input);
// Create a stream of tokens fed by the lexer
CommonTokenStream tokens = new CommonTokenStream(lexer);
// Create a parser that feeds off the token stream
ReaderParser parser = new ReaderParser(tokens);
// Begin parsing at rule prog, get return value structure
try{
	if(lexer.rex != null)
		throw lexer.rex;
	for(int i=0;i<20;i++)
		{
    		Object e = parser.expression();
    		RT.print(e,w);
    		w.write('\n');
		 w.flush();
		}
	}
catch(RecognitionException rex)
	{
	System.err.printf("Error parsing at line: \%d, col: \%d",rex.line,rex.charPositionInLine);
	}
}

protected void mismatch(IntStream input, int ttype, BitSet follow) 
throws RecognitionException 
{ 
throw new MismatchedTokenException(ttype, input); 
} 
public void recoverFromMismatchedSet(IntStream input, 
RecognitionException e, 
BitSet follow) 
throws RecognitionException 
{ 
throw e; 
} 


}

@rulecatch { 
catch (RecognitionException exc) { 
throw exc; 
} 
}

expression returns[Object val]
	:d = dotExpression {$val = $d.val;}
	| e = otherThanDotExpression {$val = $e.val;}
	;
	
fragment
otherThanDotExpression returns[Object val]
	: lt = literal {$val = $lt.val;}
	|s = symbol {$val = $s.val;}
	|k = keyword {$val = $k.val;}
	|le = listExpression {$val = $le.val;}
	|ve = vectorExpression {$val = $ve.val;}
	|me = mapExpression {$val = $me.val;}
	|mx = metaExpression {$val = $mx.val;}
	|q = quotedExpression {$val = $q.val;}
	|ct = caretExpression {$val = $ct.val;}
	;
	
listExpression returns[ISeq val]
	:	'('  es = expressions ')' {$val = es!=null?RT.seq(es):PersistentList.EMPTY;}
	;

fragment expressions returns[List es]
	: e = expression*
	;	
	
vectorExpression returns[IPersistentArray val]
	:	'[' es = expressions ']' {$val = es!=null?RT.tuple(es.toArray()):PersistentVector.EMPTY;}
	;

mapExpression returns[IPersistentMap val]
@init{
val = PersistentHashMap.EMPTY;
}
	:	'{' (k=expression v=expression)* '}'
	;

symbol returns[Symbol val]
	:n = Identifier {$val = Symbol.intern($n.text);}
	|n = NSIdentifier {$val = Symbol.intern($n.text);}
	|dd = DotDot {$val = DOTDOT;}
	;
	
keyword returns[Keyword val]
	:k = KeywordIdentifier	{$val = new Keyword(Symbol.intern($k.text.substring(1)));}
	;

			
literal returns[Object val]	
    :i = integerLiteral {$val=$i.val;}
    |fp = FloatingPointLiteral {$val=Num.from(Double.valueOf($fp.text));}
    |c = CharacterLiteral {$val=$c.text.charAt(0);}
    |s = StringLiteral {$val=$s.text.substring(1,s.getText().length()-1);}
    |TrueToken {$val=Boolean.TRUE;}
    |NullToken {$val=null;}
    |r = ratioLiteral {$val=$r.val;}
    ;

ratioLiteral returns[Num val]
	: n = DecimalLiteral '/' d = DecimalLiteral {$val = Num.divide(new BigInteger($n.text),new BigInteger($d.text));}
	;
	
integerLiteral returns[Num val]
    :   hn = HexLiteral  {$val = Num.from(new BigInteger($hn.text.substring(2),16));}  
    |   on = OctalLiteral  {$val = Num.from(new BigInteger($on.text.substring(1),8));} 
    |   nn = DecimalLiteral {$val = Num.from(new BigInteger($nn.text));}
    ;


 
fragment
metaTag returns[IPersistentMap val]
	:'#^' s = symbol {$val = RT.map(RT.TAG_KEY,$s.val);}
	|'#^' m = mapExpression {$val = $m.val;}
	;
	 
fragment
objExpression returns[Obj val]
	:s = symbol {$val = $s.val;}
	|le = listExpression {$val = (Obj)$le.val;}
	|me = mapExpression {$val = (Obj)$me.val;}
	|ve = vectorExpression {$val = (Obj)$ve.val;}
	; 
	
metaExpression returns [Obj val]
	:m = metaTag e = objExpression {$val = $e.val.withMeta($m.val);}
	;

fragment 
member returns [Object val]
	: '.' i = Identifier {$val = Symbol.intern($i.text);}
	| '.' m = method {$val = $m.val;}
	;

fragment
method returns [Object val]		
	: i = MethodIdentifier es = args? ')' {$val = RT.cons(Symbol.intern($i.text.substring(0,$i.text.length()-1)), es);}
	;

fragment args returns[ISeq val]
@init{
List es = null;
}
	: e1 = expression  (Comma e = expression)* {$val = RT.cons(e1,es!=null?RT.seq(es):null);}
	;
			
dotExpression returns [Object val]
@init{
List es = null;
}
	:s = otherThanDotExpression e = member+ {$val = RT.listStar(DOTDOT,s,RT.seq(es));}
	;	
	
quotedExpression returns[Object val]
	:'\'' e = expression {$val = RT.list(QUOTE,e);}
	;  
	
caretExpression returns[Object val]
	:'^' e = expression {$val = RT.list(META,e);}
	; 
// LEXER
Comma 	:	',';

TrueToken  :   'true';

NullToken  :   'null' ;

DotDot :	'..';
    
HexLiteral : '0' ('x'|'X') HexDigit+;

DecimalLiteral : ('0' | '1'..'9' '0'..'9'*);

OctalLiteral : '0' ('0'..'7')+;

fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;

FloatingPointLiteral
    :   ('0'..'9')+ '.' ('0'..'9')* Exponent? FloatTypeSuffix?
    |   '.' ('0'..'9')+ Exponent? FloatTypeSuffix?
    |   ('0'..'9')+ Exponent FloatTypeSuffix?
    |   ('0'..'9')+ Exponent? FloatTypeSuffix
	;

fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
FloatTypeSuffix : ('f'|'F'|'d'|'D') ;

CharacterLiteral
    :   '\\' ( EscapeSequence | ~('\\') )
    ;

StringLiteral
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

fragment
EscapeSequence
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UnicodeEscape
    |   OctalEscape
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

/*	
JavaIdentifier 
    :   Letter (Letter|JavaIDDigit)*
    ;
*/

Identifier 
    :Letter ('-' |Letter|JavaIDDigit)*
    ;
  
NSIdentifier
	:Identifier '/' Identifier
	;    
	
KeywordIdentifier
	:':' Identifier '/' Identifier
	|':' Identifier
	;
	
MethodIdentifier 
    :   Letter (Letter|JavaIDDigit)* '('
    ;
/**I found this char range in JavaCC's grammar, but Letter and Digit overlap.
   Still works, but...
 */
fragment
Letter
    :  '\u0024' |
       '\u0041'..'\u005a' |
       '\u005f' |
       '\u0061'..'\u007a' |
       '\u00c0'..'\u00d6' |
       '\u00d8'..'\u00f6' |
       '\u00f8'..'\u00ff' |
       '\u0100'..'\u1fff' |
       '\u3040'..'\u318f' |
       '\u3300'..'\u337f' |
       '\u3400'..'\u3d2d' |
       '\u4e00'..'\u9fff' |
       '\uf900'..'\ufaff'
    ;

fragment
JavaIDDigit
    :  
    	'\u0030'..'\u0039' |
       '\u0660'..'\u0669' |
       '\u06f0'..'\u06f9' |
       '\u0966'..'\u096f' |
       '\u09e6'..'\u09ef' |
       '\u0a66'..'\u0a6f' |
       '\u0ae6'..'\u0aef' |
       '\u0b66'..'\u0b6f' |
       '\u0be7'..'\u0bef' |
       '\u0c66'..'\u0c6f' |
       '\u0ce6'..'\u0cef' |
       '\u0d66'..'\u0d6f' |
       '\u0e50'..'\u0e59' |
       '\u0ed0'..'\u0ed9' |
       '\u1040'..'\u1049'
   ;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n') {$channel=HIDDEN;}
    ;

COMMENT
    :   '#|' ( options {greedy=false;} : . )* '|#' {$channel=HIDDEN;}
    ;

LINE_COMMENT
    : ';' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    ;
