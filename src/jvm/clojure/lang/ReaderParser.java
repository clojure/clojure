// $ANTLR 3.0 /Users/rich/dev/clojure/src/jvm/Reader.g 2007-07-16 11:13:34

package clojure.lang;

import java.math.BigInteger;
import java.io.Writer;
import java.io.PrintWriter;


import org.antlr.runtime.*;

import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 * which can be found in the file CPL.TXT at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */
public class ReaderParser extends Parser{
public static final String[] tokenNames = new String[]{
		"<invalid>", "<EOR>", "<DOWN>", "<UP>", "Identifier", "NSIdentifier", "DotDot", "KeywordIdentifier",
		"FloatingPointLiteral", "CharacterLiteral", "StringLiteral", "TrueToken", "NullToken", "DecimalLiteral",
		"HexLiteral", "OctalLiteral", "MethodIdentifier", "Comma", "HexDigit", "Exponent", "FloatTypeSuffix",
		"EscapeSequence", "UnicodeEscape", "OctalEscape", "Letter", "JavaIDDigit", "WS", "COMMENT", "LINE_COMMENT",
		"'('", "')'", "'['", "']'", "'{'", "'}'", "'/'", "'#^'", "'.'", "'\\''", "'^'"
};
public static final int TrueToken = 11;
public static final int Exponent = 19;
public static final int OctalLiteral = 15;
public static final int Identifier = 4;
public static final int HexDigit = 18;
public static final int WS = 26;
public static final int CharacterLiteral = 9;
public static final int MethodIdentifier = 16;
public static final int NSIdentifier = 5;
public static final int COMMENT = 27;
public static final int KeywordIdentifier = 7;
public static final int StringLiteral = 10;
public static final int LINE_COMMENT = 28;
public static final int DotDot = 6;
public static final int JavaIDDigit = 25;
public static final int Letter = 24;
public static final int UnicodeEscape = 22;
public static final int HexLiteral = 14;
public static final int Comma = 17;
public static final int EscapeSequence = 21;
public static final int EOF = -1;
public static final int NullToken = 12;
public static final int DecimalLiteral = 13;
public static final int OctalEscape = 23;
public static final int FloatingPointLiteral = 8;
public static final int FloatTypeSuffix = 20;

public ReaderParser(TokenStream input){
	super(input);
	ruleMemo = new HashMap[48 + 1];
}


public String[] getTokenNames(){
	return tokenNames;
}

public String getGrammarFileName(){
	return "/Users/rich/dev/clojure/src/jvm/Reader.g";
}

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

final static Symbol DOTDOT = new Symbol("..");
final static Symbol QUOTE = new Symbol("quote");
final static Symbol META = new Symbol("meta");

public static void main(String[] args) throws Exception{
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
	try
		{
		if(lexer.rex != null)
			throw lexer.rex;
		for(int i = 0; i < 20; i++)
			{
			Object e = parser.expression();
			RT.print(e, w);
			w.write('\n');
			w.flush();
			}
		}
	catch(RecognitionException rex)
		{
		System.err.printf("Error parsing at line: %d, col: %d", rex.line, rex.charPositionInLine);
		}
}

protected void mismatch(IntStream input, int ttype, BitSet follow)
		throws RecognitionException{
	throw new MismatchedTokenException(ttype, input);
}

public void recoverFromMismatchedSet(IntStream input,
                                     RecognitionException e,
                                     BitSet follow)
		throws RecognitionException{
	throw e;
}


// $ANTLR start expression
// /Users/rich/dev/clojure/src/jvm/Reader.g:121:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression );
public final Object expression() throws RecognitionException{
	Object val = null;
	int expression_StartIndex = input.index();
	Object lt = null;

	Symbol s = null;

	Keyword k = null;

	ISeq le = null;

	IPersistentArray ve = null;

	IPersistentMap me = null;

	Obj mx = null;

	Object g = null;

	Object q = null;

	Object ct = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 1))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:122:4: (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression )
		int alt1 = 10;
		switch(input.LA(1))
			{
			case FloatingPointLiteral:
			case CharacterLiteral:
			case StringLiteral:
			case TrueToken:
			case NullToken:
			case DecimalLiteral:
			case HexLiteral:
			case OctalLiteral:
			{
			alt1 = 1;
			}
			break;
			case Identifier:
			{
			int LA1_2 = input.LA(2);

			if((LA1_2 == EOF || (LA1_2 >= Identifier && LA1_2 <= OctalLiteral) || LA1_2 == Comma ||
			    (LA1_2 >= 29 && LA1_2 <= 34) || LA1_2 == 36 || (LA1_2 >= 38 && LA1_2 <= 39)))
				{
				alt1 = 2;
				}
			else if((LA1_2 == 37))
				{
				alt1 = 8;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"121:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression );",
								1, 2, input);

				throw nvae;
				}
			}
			break;
			case NSIdentifier:
			{
			int LA1_3 = input.LA(2);

			if((LA1_3 == EOF || (LA1_3 >= Identifier && LA1_3 <= OctalLiteral) || LA1_3 == Comma ||
			    (LA1_3 >= 29 && LA1_3 <= 34) || LA1_3 == 36 || (LA1_3 >= 38 && LA1_3 <= 39)))
				{
				alt1 = 2;
				}
			else if((LA1_3 == 37))
				{
				alt1 = 8;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"121:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression );",
								1, 3, input);

				throw nvae;
				}
			}
			break;
			case DotDot:
			{
			int LA1_4 = input.LA(2);

			if((LA1_4 == EOF || (LA1_4 >= Identifier && LA1_4 <= OctalLiteral) || LA1_4 == Comma ||
			    (LA1_4 >= 29 && LA1_4 <= 34) || LA1_4 == 36 || (LA1_4 >= 38 && LA1_4 <= 39)))
				{
				alt1 = 2;
				}
			else if((LA1_4 == 37))
				{
				alt1 = 8;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"121:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression );",
								1, 4, input);

				throw nvae;
				}
			}
			break;
			case KeywordIdentifier:
			{
			alt1 = 3;
			}
			break;
			case 29:
			{
			alt1 = 4;
			}
			break;
			case 31:
			{
			alt1 = 5;
			}
			break;
			case 33:
			{
			alt1 = 6;
			}
			break;
			case 36:
			{
			alt1 = 7;
			}
			break;
			case 38:
			{
			alt1 = 9;
			}
			break;
			case 39:
			{
			alt1 = 10;
			}
			break;
			default:
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"121:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression | q= quotedExpression | ct= caretExpression );",
								1, 0, input);

				throw nvae;
			}

		switch(alt1)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:122:4: lt= literal
			{
			pushFollow(FOLLOW_literal_in_expression73);
			lt = literal();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = lt;
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:123:3: s= symbol
			{
			pushFollow(FOLLOW_symbol_in_expression83);
			s = symbol();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = s;
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:124:3: k= keyword
			{
			pushFollow(FOLLOW_keyword_in_expression93);
			k = keyword();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = k;
				}

			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:125:3: le= listExpression
			{
			pushFollow(FOLLOW_listExpression_in_expression103);
			le = listExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = le;
				}

			}
			break;
			case 5:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:126:3: ve= vectorExpression
			{
			pushFollow(FOLLOW_vectorExpression_in_expression113);
			ve = vectorExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = ve;
				}

			}
			break;
			case 6:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:127:3: me= mapExpression
			{
			pushFollow(FOLLOW_mapExpression_in_expression123);
			me = mapExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = me;
				}

			}
			break;
			case 7:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:128:3: mx= metaExpression
			{
			pushFollow(FOLLOW_metaExpression_in_expression133);
			mx = metaExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = mx;
				}

			}
			break;
			case 8:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:129:3: g= dotExpression
			{
			pushFollow(FOLLOW_dotExpression_in_expression143);
			g = dotExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = g;
				}

			}
			break;
			case 9:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:130:3: q= quotedExpression
			{
			pushFollow(FOLLOW_quotedExpression_in_expression153);
			q = quotedExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = q;
				}

			}
			break;
			case 10:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:131:3: ct= caretExpression
			{
			pushFollow(FOLLOW_caretExpression_in_expression163);
			ct = caretExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = ct;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 1, expression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end expression

// $ANTLR start listExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:134:1: listExpression returns [ISeq val] : '(' es= expressions ')' ;

public final ISeq listExpression() throws RecognitionException{
	ISeq val = null;
	int listExpression_StartIndex = input.index();
	List es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 2))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:135:4: ( '(' es= expressions ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:135:4: '(' es= expressions ')'
		{
		match(input, 29, FOLLOW_29_in_listExpression180);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_listExpression187);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 30, FOLLOW_30_in_listExpression189);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = es != null ? RT.seq(es) : PersistentList.EMPTY;
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 2, listExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end listExpression

// $ANTLR start expressions
// /Users/rich/dev/clojure/src/jvm/Reader.g:138:10: fragment expressions returns [List es] : (e= expression )* ;

public final List expressions() throws RecognitionException{
	List es = null;
	int expressions_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 3))
			{
			return es;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:139:4: ( (e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:139:4: (e= expression )*
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:139:6: (e= expression )*
		loop2:
		do
			{
			int alt2 = 2;
			int LA2_0 = input.LA(1);

			if(((LA2_0 >= Identifier && LA2_0 <= OctalLiteral) || LA2_0 == 29 || LA2_0 == 31 || LA2_0 == 33 ||
			    LA2_0 == 36 || (LA2_0 >= 38 && LA2_0 <= 39)))
				{
				alt2 = 1;
				}


			switch(alt2)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: e= expression
				{
				pushFollow(FOLLOW_expression_in_expressions211);
				e = expression();
				_fsp--;
				if(failed) return es;

				}
				break;

				default:
					break loop2;
				}
			} while(true);


		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 3, expressions_StartIndex);
			}
		}
	return es;
}
// $ANTLR end expressions

// $ANTLR start vectorExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:142:1: vectorExpression returns [IPersistentArray val] : '[' es= expressions ']' ;

public final IPersistentArray vectorExpression() throws RecognitionException{
	IPersistentArray val = null;
	int vectorExpression_StartIndex = input.index();
	List es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 4))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:143:4: ( '[' es= expressions ']' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:143:4: '[' es= expressions ']'
		{
		match(input, 31, FOLLOW_31_in_vectorExpression228);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_vectorExpression234);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 32, FOLLOW_32_in_vectorExpression236);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = es != null ? RT.tuple(es.toArray()) : PersistentVector.EMPTY;
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 4, vectorExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end vectorExpression

// $ANTLR start mapExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:146:1: mapExpression returns [IPersistentMap val] : '{' (k= expression v= expression )* '}' ;

public final IPersistentMap mapExpression() throws RecognitionException{
	IPersistentMap val = null;
	int mapExpression_StartIndex = input.index();
	Object k = null;

	Object v = null;


	val = PersistentHashMap.EMPTY;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 5))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:150:4: ( '{' (k= expression v= expression )* '}' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:150:4: '{' (k= expression v= expression )* '}'
		{
		match(input, 33, FOLLOW_33_in_mapExpression256);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:150:8: (k= expression v= expression )*
		loop3:
		do
			{
			int alt3 = 2;
			int LA3_0 = input.LA(1);

			if(((LA3_0 >= Identifier && LA3_0 <= OctalLiteral) || LA3_0 == 29 || LA3_0 == 31 || LA3_0 == 33 ||
			    LA3_0 == 36 || (LA3_0 >= 38 && LA3_0 <= 39)))
				{
				alt3 = 1;
				}


			switch(alt3)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:150:9: k= expression v= expression
				{
				pushFollow(FOLLOW_expression_in_mapExpression261);
				k = expression();
				_fsp--;
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_mapExpression265);
				v = expression();
				_fsp--;
				if(failed) return val;

				}
				break;

				default:
					break loop3;
				}
			} while(true);

		match(input, 34, FOLLOW_34_in_mapExpression269);
		if(failed) return val;

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 5, mapExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end mapExpression

// $ANTLR start symbol
// /Users/rich/dev/clojure/src/jvm/Reader.g:153:1: symbol returns [Symbol val] : (n= Identifier | n= NSIdentifier | dd= DotDot );

public final Symbol symbol() throws RecognitionException{
	Symbol val = null;
	int symbol_StartIndex = input.index();
	Token n = null;
	Token dd = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 6))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:154:3: (n= Identifier | n= NSIdentifier | dd= DotDot )
		int alt4 = 3;
		switch(input.LA(1))
			{
			case Identifier:
			{
			alt4 = 1;
			}
			break;
			case NSIdentifier:
			{
			alt4 = 2;
			}
			break;
			case DotDot:
			{
			alt4 = 3;
			}
			break;
			default:
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"153:1: symbol returns [Symbol val] : (n= Identifier | n= NSIdentifier | dd= DotDot );",
								4, 0, input);

				throw nvae;
			}

		switch(alt4)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:154:3: n= Identifier
			{
			n = (Token) input.LT(1);
			match(input, Identifier, FOLLOW_Identifier_in_symbol286);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(n.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:155:3: n= NSIdentifier
			{
			n = (Token) input.LT(1);
			match(input, NSIdentifier, FOLLOW_NSIdentifier_in_symbol296);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(n.getText());
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:156:3: dd= DotDot
			{
			dd = (Token) input.LT(1);
			match(input, DotDot, FOLLOW_DotDot_in_symbol306);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = DOTDOT;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 6, symbol_StartIndex);
			}
		}
	return val;
}
// $ANTLR end symbol

// $ANTLR start keyword
// /Users/rich/dev/clojure/src/jvm/Reader.g:159:1: keyword returns [Keyword val] : k= KeywordIdentifier ;

public final Keyword keyword() throws RecognitionException{
	Keyword val = null;
	int keyword_StartIndex = input.index();
	Token k = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 7))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:160:3: (k= KeywordIdentifier )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:160:3: k= KeywordIdentifier
		{
		k = (Token) input.LT(1);
		match(input, KeywordIdentifier, FOLLOW_KeywordIdentifier_in_keyword326);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = new Keyword(k.getText().substring(1));
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 7, keyword_StartIndex);
			}
		}
	return val;
}
// $ANTLR end keyword

// $ANTLR start literal
// /Users/rich/dev/clojure/src/jvm/Reader.g:164:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );

public final Object literal() throws RecognitionException{
	Object val = null;
	int literal_StartIndex = input.index();
	Token fp = null;
	Token c = null;
	Token s = null;
	Num i = null;

	Num r = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 8))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:165:6: (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral )
		int alt5 = 7;
		switch(input.LA(1))
			{
			case HexLiteral:
			case OctalLiteral:
			{
			alt5 = 1;
			}
			break;
			case DecimalLiteral:
			{
			int LA5_2 = input.LA(2);

			if((LA5_2 == 35))
				{
				alt5 = 7;
				}
			else if((LA5_2 == EOF || (LA5_2 >= Identifier && LA5_2 <= OctalLiteral) || LA5_2 == Comma ||
			         (LA5_2 >= 29 && LA5_2 <= 34) || LA5_2 == 36 || (LA5_2 >= 38 && LA5_2 <= 39)))
				{
				alt5 = 1;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"164:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
								5, 2, input);

				throw nvae;
				}
			}
			break;
			case FloatingPointLiteral:
			{
			alt5 = 2;
			}
			break;
			case CharacterLiteral:
			{
			alt5 = 3;
			}
			break;
			case StringLiteral:
			{
			alt5 = 4;
			}
			break;
			case TrueToken:
			{
			alt5 = 5;
			}
			break;
			case NullToken:
			{
			alt5 = 6;
			}
			break;
			default:
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"164:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
								5, 0, input);

				throw nvae;
			}

		switch(alt5)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:165:6: i= integerLiteral
			{
			pushFollow(FOLLOW_integerLiteral_in_literal353);
			i = integerLiteral();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = i;
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:166:6: fp= FloatingPointLiteral
			{
			fp = (Token) input.LT(1);
			match(input, FloatingPointLiteral, FOLLOW_FloatingPointLiteral_in_literal366);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(Double.valueOf(fp.getText()));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:167:6: c= CharacterLiteral
			{
			c = (Token) input.LT(1);
			match(input, CharacterLiteral, FOLLOW_CharacterLiteral_in_literal379);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = c.getText().charAt(0);
				}

			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:168:6: s= StringLiteral
			{
			s = (Token) input.LT(1);
			match(input, StringLiteral, FOLLOW_StringLiteral_in_literal392);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = s.getText().substring(1, s.getText().length() - 1);
				}

			}
			break;
			case 5:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:169:6: TrueToken
			{
			match(input, TrueToken, FOLLOW_TrueToken_in_literal401);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Boolean.TRUE;
				}

			}
			break;
			case 6:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:170:6: NullToken
			{
			match(input, NullToken, FOLLOW_NullToken_in_literal410);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = null;
				}

			}
			break;
			case 7:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:171:6: r= ratioLiteral
			{
			pushFollow(FOLLOW_ratioLiteral_in_literal423);
			r = ratioLiteral();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = r;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 8, literal_StartIndex);
			}
		}
	return val;
}
// $ANTLR end literal

// $ANTLR start ratioLiteral
// /Users/rich/dev/clojure/src/jvm/Reader.g:174:1: ratioLiteral returns [Num val] : n= DecimalLiteral '/' d= DecimalLiteral ;

public final Num ratioLiteral() throws RecognitionException{
	Num val = null;
	int ratioLiteral_StartIndex = input.index();
	Token n = null;
	Token d = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 9))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:175:4: (n= DecimalLiteral '/' d= DecimalLiteral )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:175:4: n= DecimalLiteral '/' d= DecimalLiteral
		{
		n = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral446);
		if(failed) return val;
		match(input, 35, FOLLOW_35_in_ratioLiteral448);
		if(failed) return val;
		d = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral454);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = Num.divide(new BigInteger(n.getText()), new BigInteger(d.getText()));
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 9, ratioLiteral_StartIndex);
			}
		}
	return val;
}
// $ANTLR end ratioLiteral

// $ANTLR start integerLiteral
// /Users/rich/dev/clojure/src/jvm/Reader.g:178:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );

public final Num integerLiteral() throws RecognitionException{
	Num val = null;
	int integerLiteral_StartIndex = input.index();
	Token hn = null;
	Token on = null;
	Token nn = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 10))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:179:9: (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral )
		int alt6 = 3;
		switch(input.LA(1))
			{
			case HexLiteral:
			{
			alt6 = 1;
			}
			break;
			case OctalLiteral:
			{
			alt6 = 2;
			}
			break;
			case DecimalLiteral:
			{
			alt6 = 3;
			}
			break;
			default:
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"178:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );",
								6, 0, input);

				throw nvae;
			}

		switch(alt6)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:179:9: hn= HexLiteral
			{
			hn = (Token) input.LT(1);
			match(input, HexLiteral, FOLLOW_HexLiteral_in_integerLiteral480);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(hn.getText().substring(2), 16));
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:180:9: on= OctalLiteral
			{
			on = (Token) input.LT(1);
			match(input, OctalLiteral, FOLLOW_OctalLiteral_in_integerLiteral499);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(on.getText().substring(1), 8));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:181:9: nn= DecimalLiteral
			{
			nn = (Token) input.LT(1);
			match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_integerLiteral517);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(nn.getText()));
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 10, integerLiteral_StartIndex);
			}
		}
	return val;
}
// $ANTLR end integerLiteral

// $ANTLR start metaTag
// /Users/rich/dev/clojure/src/jvm/Reader.g:186:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );

public final IPersistentMap metaTag() throws RecognitionException{
	IPersistentMap val = null;
	int metaTag_StartIndex = input.index();
	Symbol s = null;

	IPersistentMap m = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 11))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:188:3: ( '#^' s= symbol | '#^' m= mapExpression )
		int alt7 = 2;
		int LA7_0 = input.LA(1);

		if((LA7_0 == 36))
			{
			int LA7_1 = input.LA(2);

			if((LA7_1 == 33))
				{
				alt7 = 2;
				}
			else if(((LA7_1 >= Identifier && LA7_1 <= DotDot)))
				{
				alt7 = 1;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"186:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
								7, 1, input);

				throw nvae;
				}
			}
		else
			{
			if(backtracking > 0)
				{
				failed = true;
				return val;
				}
			NoViableAltException nvae =
					new NoViableAltException(
							"186:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
							7, 0, input);

			throw nvae;
			}
		switch(alt7)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:188:3: '#^' s= symbol
			{
			match(input, 36, FOLLOW_36_in_metaTag540);
			if(failed) return val;
			pushFollow(FOLLOW_symbol_in_metaTag546);
			s = symbol();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = RT.map(RT.TAG_KEY, s);
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:189:3: '#^' m= mapExpression
			{
			match(input, 36, FOLLOW_36_in_metaTag552);
			if(failed) return val;
			pushFollow(FOLLOW_mapExpression_in_metaTag558);
			m = mapExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = m;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 11, metaTag_StartIndex);
			}
		}
	return val;
}
// $ANTLR end metaTag

// $ANTLR start objExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:192:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );

public final Obj objExpression() throws RecognitionException{
	Obj val = null;
	int objExpression_StartIndex = input.index();
	Symbol s = null;

	ISeq le = null;

	IPersistentMap me = null;

	IPersistentArray ve = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 12))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:194:3: (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression )
		int alt8 = 4;
		switch(input.LA(1))
			{
			case Identifier:
			case NSIdentifier:
			case DotDot:
			{
			alt8 = 1;
			}
			break;
			case 29:
			{
			alt8 = 2;
			}
			break;
			case 33:
			{
			alt8 = 3;
			}
			break;
			case 31:
			{
			alt8 = 4;
			}
			break;
			default:
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"192:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );",
								8, 0, input);

				throw nvae;
			}

		switch(alt8)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:194:3: s= symbol
			{
			pushFollow(FOLLOW_symbol_in_objExpression581);
			s = symbol();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = s;
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:195:3: le= listExpression
			{
			pushFollow(FOLLOW_listExpression_in_objExpression591);
			le = listExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = (Obj) le;
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:196:3: me= mapExpression
			{
			pushFollow(FOLLOW_mapExpression_in_objExpression601);
			me = mapExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = (Obj) me;
				}

			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:197:3: ve= vectorExpression
			{
			pushFollow(FOLLOW_vectorExpression_in_objExpression611);
			ve = vectorExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = (Obj) ve;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 12, objExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end objExpression

// $ANTLR start metaExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:200:1: metaExpression returns [Obj val] : m= metaTag e= objExpression ;

public final Obj metaExpression() throws RecognitionException{
	Obj val = null;
	int metaExpression_StartIndex = input.index();
	IPersistentMap m = null;

	Obj e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 13))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:201:3: (m= metaTag e= objExpression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:201:3: m= metaTag e= objExpression
		{
		pushFollow(FOLLOW_metaTag_in_metaExpression633);
		m = metaTag();
		_fsp--;
		if(failed) return val;
		pushFollow(FOLLOW_objExpression_in_metaExpression639);
		e = objExpression();
		_fsp--;
		if(failed) return val;
		if(backtracking == 0)
			{
			val = e.withMeta(m);
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 13, metaExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end metaExpression

// $ANTLR start member
// /Users/rich/dev/clojure/src/jvm/Reader.g:204:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );

public final Object member() throws RecognitionException{
	Object val = null;
	int member_StartIndex = input.index();
	Token i = null;
	Object m = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 14))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:206:4: ( '.' i= Identifier | '.' m= method )
		int alt9 = 2;
		int LA9_0 = input.LA(1);

		if((LA9_0 == 37))
			{
			int LA9_1 = input.LA(2);

			if((LA9_1 == Identifier))
				{
				alt9 = 1;
				}
			else if((LA9_1 == MethodIdentifier))
				{
				alt9 = 2;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				NoViableAltException nvae =
						new NoViableAltException(
								"204:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );",
								9, 1, input);

				throw nvae;
				}
			}
		else
			{
			if(backtracking > 0)
				{
				failed = true;
				return val;
				}
			NoViableAltException nvae =
					new NoViableAltException(
							"204:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );", 9,
							0, input);

			throw nvae;
			}
		switch(alt9)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:206:4: '.' i= Identifier
			{
			match(input, 37, FOLLOW_37_in_member659);
			if(failed) return val;
			i = (Token) input.LT(1);
			match(input, Identifier, FOLLOW_Identifier_in_member665);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(i.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:207:4: '.' m= method
			{
			match(input, 37, FOLLOW_37_in_member672);
			if(failed) return val;
			pushFollow(FOLLOW_method_in_member678);
			m = method();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = m;
				}

			}
			break;

			}
		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 14, member_StartIndex);
			}
		}
	return val;
}
// $ANTLR end member

// $ANTLR start method
// /Users/rich/dev/clojure/src/jvm/Reader.g:210:1: fragment method returns [Object val] : i= MethodIdentifier (es= args )? ')' ;

public final Object method() throws RecognitionException{
	Object val = null;
	int method_StartIndex = input.index();
	Token i = null;
	ISeq es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 15))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:212:4: (i= MethodIdentifier (es= args )? ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:212:4: i= MethodIdentifier (es= args )? ')'
		{
		i = (Token) input.LT(1);
		match(input, MethodIdentifier, FOLLOW_MethodIdentifier_in_method703);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:212:28: (es= args )?
		int alt10 = 2;
		int LA10_0 = input.LA(1);

		if(((LA10_0 >= Identifier && LA10_0 <= OctalLiteral) || LA10_0 == 29 || LA10_0 == 31 || LA10_0 == 33 ||
		    LA10_0 == 36 || (LA10_0 >= 38 && LA10_0 <= 39)))
			{
			alt10 = 1;
			}
		switch(alt10)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: es= args
			{
			pushFollow(FOLLOW_args_in_method709);
			es = args();
			_fsp--;
			if(failed) return val;

			}
			break;

			}

		match(input, 30, FOLLOW_30_in_method712);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = RT.cons(new Symbol(i.getText().substring(0, i.getText().length() - 1)), es);
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 15, method_StartIndex);
			}
		}
	return val;
}
// $ANTLR end method

// $ANTLR start args
// /Users/rich/dev/clojure/src/jvm/Reader.g:215:10: fragment args returns [ISeq val] : e1= expression ( Comma e= expression )* ;

public final ISeq args() throws RecognitionException{
	ISeq val = null;
	int args_StartIndex = input.index();
	Object e1 = null;

	Object e = null;


	List es = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 16))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:4: (e1= expression ( Comma e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:4: e1= expression ( Comma e= expression )*
		{
		pushFollow(FOLLOW_expression_in_args738);
		e1 = expression();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:21: ( Comma e= expression )*
		loop11:
		do
			{
			int alt11 = 2;
			int LA11_0 = input.LA(1);

			if((LA11_0 == Comma))
				{
				alt11 = 1;
				}


			switch(alt11)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:219:22: Comma e= expression
				{
				match(input, Comma, FOLLOW_Comma_in_args742);
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_args748);
				e = expression();
				_fsp--;
				if(failed) return val;

				}
				break;

				default:
					break loop11;
				}
			} while(true);

		if(backtracking == 0)
			{
			val = RT.cons(e1, es != null ? RT.seq(es) : null);
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 16, args_StartIndex);
			}
		}
	return val;
}
// $ANTLR end args

// $ANTLR start dotExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:222:1: dotExpression returns [Object val] : s= symbol (e= member )+ ;

public final Object dotExpression() throws RecognitionException{
	Object val = null;
	int dotExpression_StartIndex = input.index();
	Symbol s = null;

	Object e = null;


	List es = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 17))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:226:3: (s= symbol (e= member )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:226:3: s= symbol (e= member )+
		{
		pushFollow(FOLLOW_symbol_in_dotExpression777);
		s = symbol();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:226:16: (e= member )+
		int cnt12 = 0;
		loop12:
		do
			{
			int alt12 = 2;
			int LA12_0 = input.LA(1);

			if((LA12_0 == 37))
				{
				alt12 = 1;
				}


			switch(alt12)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: e= member
				{
				pushFollow(FOLLOW_member_in_dotExpression783);
				e = member();
				_fsp--;
				if(failed) return val;

				}
				break;

				default:
					if(cnt12 >= 1) break loop12;
					if(backtracking > 0)
						{
						failed = true;
						return val;
						}
					EarlyExitException eee =
							new EarlyExitException(12, input);
					throw eee;
				}
			cnt12++;
			} while(true);

		if(backtracking == 0)
			{
			val = RT.listStar(DOTDOT, s, RT.seq(es));
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 17, dotExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end dotExpression

// $ANTLR start quotedExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:229:1: quotedExpression returns [Object val] : '\\'' e= expression ;

public final Object quotedExpression() throws RecognitionException{
	Object val = null;
	int quotedExpression_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 18))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:230:3: ( '\\'' e= expression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:230:3: '\\'' e= expression
		{
		match(input, 38, FOLLOW_38_in_quotedExpression801);
		if(failed) return val;
		pushFollow(FOLLOW_expression_in_quotedExpression807);
		e = expression();
		_fsp--;
		if(failed) return val;
		if(backtracking == 0)
			{
			val = RT.list(QUOTE, e);
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 18, quotedExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end quotedExpression

// $ANTLR start caretExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:233:1: caretExpression returns [Object val] : '^' e= expression ;

public final Object caretExpression() throws RecognitionException{
	Object val = null;
	int caretExpression_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 19))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:234:3: ( '^' e= expression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:234:3: '^' e= expression
		{
		match(input, 39, FOLLOW_39_in_caretExpression825);
		if(failed) return val;
		pushFollow(FOLLOW_expression_in_caretExpression831);
		e = expression();
		_fsp--;
		if(failed) return val;
		if(backtracking == 0)
			{
			val = RT.list(META, e);
			}

		}

		}

	catch(RecognitionException exc)
		{
		throw exc;
		}
	finally
		{
		if(backtracking > 0)
			{
			memoize(input, 19, caretExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end caretExpression


public static final BitSet FOLLOW_literal_in_expression73 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_expression83 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_keyword_in_expression93 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_expression103 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_expression113 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_expression123 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaExpression_in_expression133 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_dotExpression_in_expression143 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_quotedExpression_in_expression153 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_caretExpression_in_expression163 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_29_in_listExpression180 = new BitSet(new long[]{0x000000D2E000FFF0L});
public static final BitSet FOLLOW_expressions_in_listExpression187 = new BitSet(new long[]{0x0000000040000000L});
public static final BitSet FOLLOW_30_in_listExpression189 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_expressions211 = new BitSet(new long[]{0x000000D2A000FFF2L});
public static final BitSet FOLLOW_31_in_vectorExpression228 = new BitSet(new long[]{0x000000D3A000FFF0L});
public static final BitSet FOLLOW_expressions_in_vectorExpression234 = new BitSet(new long[]{0x0000000100000000L});
public static final BitSet FOLLOW_32_in_vectorExpression236 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_33_in_mapExpression256 = new BitSet(new long[]{0x000000D6A000FFF0L});
public static final BitSet FOLLOW_expression_in_mapExpression261 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_mapExpression265 = new BitSet(new long[]{0x000000D6A000FFF0L});
public static final BitSet FOLLOW_34_in_mapExpression269 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_Identifier_in_symbol286 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_NSIdentifier_in_symbol296 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DotDot_in_symbol306 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_KeywordIdentifier_in_keyword326 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_integerLiteral_in_literal353 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_FloatingPointLiteral_in_literal366 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_CharacterLiteral_in_literal379 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_StringLiteral_in_literal392 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_TrueToken_in_literal401 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_NullToken_in_literal410 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_ratioLiteral_in_literal423 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral446 = new BitSet(new long[]{0x0000000800000000L});
public static final BitSet FOLLOW_35_in_ratioLiteral448 = new BitSet(new long[]{0x0000000000002000L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral454 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_HexLiteral_in_integerLiteral480 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_OctalLiteral_in_integerLiteral499 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_integerLiteral517 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag540 = new BitSet(new long[]{0x0000000000000070L});
public static final BitSet FOLLOW_symbol_in_metaTag546 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag552 = new BitSet(new long[]{0x0000000200000000L});
public static final BitSet FOLLOW_mapExpression_in_metaTag558 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_objExpression581 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_objExpression591 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_objExpression601 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_objExpression611 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaTag_in_metaExpression633 = new BitSet(new long[]{0x00000002A0000070L});
public static final BitSet FOLLOW_objExpression_in_metaExpression639 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member659 = new BitSet(new long[]{0x0000000000000010L});
public static final BitSet FOLLOW_Identifier_in_member665 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member672 = new BitSet(new long[]{0x0000000000010000L});
public static final BitSet FOLLOW_method_in_member678 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_MethodIdentifier_in_method703 = new BitSet(new long[]{0x000000D2E000FFF0L});
public static final BitSet FOLLOW_args_in_method709 = new BitSet(new long[]{0x0000000040000000L});
public static final BitSet FOLLOW_30_in_method712 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_args738 = new BitSet(new long[]{0x0000000000020002L});
public static final BitSet FOLLOW_Comma_in_args742 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_args748 = new BitSet(new long[]{0x0000000000020002L});
public static final BitSet FOLLOW_symbol_in_dotExpression777 = new BitSet(new long[]{0x0000002000000000L});
public static final BitSet FOLLOW_member_in_dotExpression783 = new BitSet(new long[]{0x0000002000000002L});
public static final BitSet FOLLOW_38_in_quotedExpression801 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_quotedExpression807 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_39_in_caretExpression825 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_caretExpression831 = new BitSet(new long[]{0x0000000000000002L});

}