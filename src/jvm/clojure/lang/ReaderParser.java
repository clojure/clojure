// $ANTLR 3.0 /Users/rich/dev/clojure/src/jvm/Reader.g 2007-07-13 12:56:20

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
		"<invalid>", "<EOR>", "<DOWN>", "<UP>", "Identifier", "JavaIdentifier", "DotDot", "FloatingPointLiteral",
		"CharacterLiteral", "StringLiteral", "TrueToken", "NullToken", "DecimalLiteral", "HexLiteral", "OctalLiteral",
		"MethodIdentifier", "Comma", "HexDigit", "Exponent", "FloatTypeSuffix", "EscapeSequence", "UnicodeEscape",
		"OctalEscape", "Letter", "JavaIDDigit", "WS", "COMMENT", "LINE_COMMENT", "'('", "')'", "'['", "']'", "'{'",
		"'}'", "'/'", "':'", "'#^'", "'.'"
};
public static final int TrueToken = 10;
public static final int Exponent = 18;
public static final int OctalLiteral = 14;
public static final int Identifier = 4;
public static final int HexDigit = 17;
public static final int WS = 25;
public static final int CharacterLiteral = 8;
public static final int MethodIdentifier = 15;
public static final int COMMENT = 26;
public static final int StringLiteral = 9;
public static final int LINE_COMMENT = 27;
public static final int DotDot = 6;
public static final int JavaIDDigit = 24;
public static final int Letter = 23;
public static final int UnicodeEscape = 21;
public static final int HexLiteral = 13;
public static final int Comma = 16;
public static final int EscapeSequence = 20;
public static final int EOF = -1;
public static final int NullToken = 11;
public static final int DecimalLiteral = 12;
public static final int OctalEscape = 22;
public static final int FloatingPointLiteral = 7;
public static final int JavaIdentifier = 5;
public static final int FloatTypeSuffix = 19;

public ReaderParser(TokenStream input){
	super(input);
	ruleMemo = new HashMap[47 + 1];
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

/*WARNING- HAND MODIFIED!
	if(es == null) es = new ArrayList();
	es.add(e);
	// END HAND MODIFIED*/

//add this to mapexpr:
/*WARNING- HAND MODIFIED!
						val = val.assoc(k, v);
	// END HAND MODIFIED*/

final static Symbol DOTDOT = new Symbol("..");

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
		for(int i = 0; i < 10; i++)
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );
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


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 1))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:107:4: (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression )
		int alt1 = 8;
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
			case JavaIdentifier:
			{
			switch(input.LA(2))
				{
				case 34:
				{
				int LA1_9 = input.LA(3);

				if(((LA1_9 >= Identifier && LA1_9 <= JavaIdentifier)))
					{
					int LA1_12 = input.LA(4);

					if((LA1_12 == 37))
						{
						alt1 = 8;
						}
					else if((LA1_12 == EOF || (LA1_12 >= Identifier && LA1_12 <= OctalLiteral) || LA1_12 == Comma ||
					         (LA1_12 >= 28 && LA1_12 <= 33) || (LA1_12 >= 35 && LA1_12 <= 36)))
						{
						alt1 = 2;
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
										"106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );",
										1, 12, input);

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
									"106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );",
									1, 9, input);

					throw nvae;
					}
				}
				break;
				case 37:
				{
				alt1 = 8;
				}
				break;
				case EOF:
				case Identifier:
				case JavaIdentifier:
				case DotDot:
				case FloatingPointLiteral:
				case CharacterLiteral:
				case StringLiteral:
				case TrueToken:
				case NullToken:
				case DecimalLiteral:
				case HexLiteral:
				case OctalLiteral:
				case Comma:
				case 28:
				case 29:
				case 30:
				case 31:
				case 32:
				case 33:
				case 35:
				case 36:
				{
				alt1 = 2;
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
									"106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );",
									1, 2, input);

					throw nvae;
				}

			}
			break;
			case DotDot:
			{
			int LA1_3 = input.LA(2);

			if((LA1_3 == 37))
				{
				alt1 = 8;
				}
			else if((LA1_3 == EOF || (LA1_3 >= Identifier && LA1_3 <= OctalLiteral) || LA1_3 == Comma ||
			         (LA1_3 >= 28 && LA1_3 <= 33) || (LA1_3 >= 35 && LA1_3 <= 36)))
				{
				alt1 = 2;
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
								"106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );",
								1, 3, input);

				throw nvae;
				}
			}
			break;
			case 35:
			{
			alt1 = 3;
			}
			break;
			case 28:
			{
			alt1 = 4;
			}
			break;
			case 30:
			{
			alt1 = 5;
			}
			break;
			case 32:
			{
			alt1 = 6;
			}
			break;
			case 36:
			{
			alt1 = 7;
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
								"106:1: expression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | g= dotExpression );",
								1, 0, input);

				throw nvae;
			}

		switch(alt1)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:107:4: lt= literal
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:108:3: s= symbol
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:109:3: k= keyword
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:110:3: le= listExpression
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:111:3: ve= vectorExpression
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:112:3: me= mapExpression
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:113:3: mx= metaExpression
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:114:3: g= dotExpression
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:117:1: listExpression returns [ISeq val] : '(' es= expressions ')' ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:118:4: ( '(' es= expressions ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:118:4: '(' es= expressions ')'
		{
		match(input, 28, FOLLOW_28_in_listExpression160);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_listExpression167);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 29, FOLLOW_29_in_listExpression169);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:121:10: fragment expressions returns [List es] : (e= expression )* ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:122:4: ( (e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:122:4: (e= expression )*
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:122:6: (e= expression )*
		loop2:
		do
			{
			int alt2 = 2;
			int LA2_0 = input.LA(1);

			if(((LA2_0 >= Identifier && LA2_0 <= OctalLiteral) || LA2_0 == 28 || LA2_0 == 30 || LA2_0 == 32 ||
			    (LA2_0 >= 35 && LA2_0 <= 36)))
				{
				alt2 = 1;
				}


			switch(alt2)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: e= expression
				{
				pushFollow(FOLLOW_expression_in_expressions191);
				e = expression();
				_fsp--;
				if(failed) return es;
				//*WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED*/

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
// /Users/rich/dev/clojure/src/jvm/Reader.g:125:1: vectorExpression returns [IPersistentArray val] : '[' es= expressions ']' ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:126:4: ( '[' es= expressions ']' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:126:4: '[' es= expressions ']'
		{
		match(input, 30, FOLLOW_30_in_vectorExpression208);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_vectorExpression214);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 31, FOLLOW_31_in_vectorExpression216);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:129:1: mapExpression returns [IPersistentMap val] : '{' (k= expression v= expression )* '}' ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:133:4: ( '{' (k= expression v= expression )* '}' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:133:4: '{' (k= expression v= expression )* '}'
		{
		match(input, 32, FOLLOW_32_in_mapExpression236);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:133:8: (k= expression v= expression )*
		loop3:
		do
			{
			int alt3 = 2;
			int LA3_0 = input.LA(1);

			if(((LA3_0 >= Identifier && LA3_0 <= OctalLiteral) || LA3_0 == 28 || LA3_0 == 30 || LA3_0 == 32 ||
			    (LA3_0 >= 35 && LA3_0 <= 36)))
				{
				alt3 = 1;
				}


			switch(alt3)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:133:9: k= expression v= expression
				{
				pushFollow(FOLLOW_expression_in_mapExpression241);
				k = expression();
				_fsp--;
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_mapExpression245);
				v = expression();
				_fsp--;
				if(failed) return val;
				//*WARNING- HAND MODIFIED!
				val = val.assoc(k, v);
				// END HAND MODIFIED*/

				}
				break;

				default:
					break loop3;
				}
			} while(true);

		match(input, 33, FOLLOW_33_in_mapExpression249);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:136:1: symbol returns [Symbol val] : (n= ( Identifier | JavaIdentifier ) | ns= ( Identifier | JavaIdentifier ) '/' nn= ( Identifier | JavaIdentifier ) | dd= DotDot );

public final Symbol symbol() throws RecognitionException{
	Symbol val = null;
	int symbol_StartIndex = input.index();
	Token n = null;
	Token ns = null;
	Token nn = null;
	Token dd = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 6))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:137:3: (n= ( Identifier | JavaIdentifier ) | ns= ( Identifier | JavaIdentifier ) '/' nn= ( Identifier | JavaIdentifier ) | dd= DotDot )
		int alt4 = 3;
		int LA4_0 = input.LA(1);

		if(((LA4_0 >= Identifier && LA4_0 <= JavaIdentifier)))
			{
			int LA4_1 = input.LA(2);

			if((LA4_1 == 34))
				{
				alt4 = 2;
				}
			else if((LA4_1 == EOF || (LA4_1 >= Identifier && LA4_1 <= OctalLiteral) || LA4_1 == Comma ||
			         (LA4_1 >= 28 && LA4_1 <= 33) || (LA4_1 >= 35 && LA4_1 <= 37)))
				{
				alt4 = 1;
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
								"136:1: symbol returns [Symbol val] : (n= ( Identifier | JavaIdentifier ) | ns= ( Identifier | JavaIdentifier ) '/' nn= ( Identifier | JavaIdentifier ) | dd= DotDot );",
								4, 1, input);

				throw nvae;
				}
			}
		else if((LA4_0 == DotDot))
			{
			alt4 = 3;
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
							"136:1: symbol returns [Symbol val] : (n= ( Identifier | JavaIdentifier ) | ns= ( Identifier | JavaIdentifier ) '/' nn= ( Identifier | JavaIdentifier ) | dd= DotDot );",
							4, 0, input);

			throw nvae;
			}
		switch(alt4)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:137:3: n= ( Identifier | JavaIdentifier )
			{
			n = (Token) input.LT(1);
			if((input.LA(1) >= Identifier && input.LA(1) <= JavaIdentifier))
				{
				input.consume();
				errorRecovery = false;
				failed = false;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recoverFromMismatchedSet(input, mse, FOLLOW_set_in_symbol266);
				throw mse;
				}

			if(backtracking == 0)
				{
				val = new Symbol(n.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:138:3: ns= ( Identifier | JavaIdentifier ) '/' nn= ( Identifier | JavaIdentifier )
			{
			ns = (Token) input.LT(1);
			if((input.LA(1) >= Identifier && input.LA(1) <= JavaIdentifier))
				{
				input.consume();
				errorRecovery = false;
				failed = false;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recoverFromMismatchedSet(input, mse, FOLLOW_set_in_symbol280);
				throw mse;
				}

			match(input, 34, FOLLOW_34_in_symbol286);
			if(failed) return val;
			nn = (Token) input.LT(1);
			if((input.LA(1) >= Identifier && input.LA(1) <= JavaIdentifier))
				{
				input.consume();
				errorRecovery = false;
				failed = false;
				}
			else
				{
				if(backtracking > 0)
					{
					failed = true;
					return val;
					}
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recoverFromMismatchedSet(input, mse, FOLLOW_set_in_symbol292);
				throw mse;
				}

			if(backtracking == 0)
				{
				val = new Symbol(ns.getText(), nn.getText());
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:139:3: dd= DotDot
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:142:1: keyword returns [Keyword val] : ':' s= symbol ;

public final Keyword keyword() throws RecognitionException{
	Keyword val = null;
	int keyword_StartIndex = input.index();
	Symbol s = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 7))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:143:3: ( ':' s= symbol )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:143:3: ':' s= symbol
		{
		match(input, 35, FOLLOW_35_in_keyword322);
		if(failed) return val;
		pushFollow(FOLLOW_symbol_in_keyword328);
		s = symbol();
		_fsp--;
		if(failed) return val;
		if(backtracking == 0)
			{
			val = new Keyword(s);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:147:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:148:6: (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral )
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

			if((LA5_2 == 34))
				{
				alt5 = 7;
				}
			else if((LA5_2 == EOF || (LA5_2 >= Identifier && LA5_2 <= OctalLiteral) || LA5_2 == Comma ||
			         (LA5_2 >= 28 && LA5_2 <= 33) || (LA5_2 >= 35 && LA5_2 <= 36)))
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
								"147:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
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
								"147:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
								5, 0, input);

				throw nvae;
			}

		switch(alt5)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:148:6: i= integerLiteral
			{
			pushFollow(FOLLOW_integerLiteral_in_literal355);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:149:6: fp= FloatingPointLiteral
			{
			fp = (Token) input.LT(1);
			match(input, FloatingPointLiteral, FOLLOW_FloatingPointLiteral_in_literal368);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(Double.valueOf(fp.getText()));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:150:6: c= CharacterLiteral
			{
			c = (Token) input.LT(1);
			match(input, CharacterLiteral, FOLLOW_CharacterLiteral_in_literal381);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = c.getText().charAt(0);
				}

			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:151:6: s= StringLiteral
			{
			s = (Token) input.LT(1);
			match(input, StringLiteral, FOLLOW_StringLiteral_in_literal394);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = s.getText().substring(1, s.getText().length() - 1);
				}

			}
			break;
			case 5:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:152:6: TrueToken
			{
			match(input, TrueToken, FOLLOW_TrueToken_in_literal403);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Boolean.TRUE;
				}

			}
			break;
			case 6:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:153:6: NullToken
			{
			match(input, NullToken, FOLLOW_NullToken_in_literal412);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = null;
				}

			}
			break;
			case 7:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:154:6: r= ratioLiteral
			{
			pushFollow(FOLLOW_ratioLiteral_in_literal425);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:157:1: ratioLiteral returns [Num val] : n= DecimalLiteral '/' d= DecimalLiteral ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:158:4: (n= DecimalLiteral '/' d= DecimalLiteral )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:158:4: n= DecimalLiteral '/' d= DecimalLiteral
		{
		n = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral448);
		if(failed) return val;
		match(input, 34, FOLLOW_34_in_ratioLiteral450);
		if(failed) return val;
		d = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral456);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:161:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:162:9: (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral )
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
								"161:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );",
								6, 0, input);

				throw nvae;
			}

		switch(alt6)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:162:9: hn= HexLiteral
			{
			hn = (Token) input.LT(1);
			match(input, HexLiteral, FOLLOW_HexLiteral_in_integerLiteral482);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(hn.getText().substring(2), 16));
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:163:9: on= OctalLiteral
			{
			on = (Token) input.LT(1);
			match(input, OctalLiteral, FOLLOW_OctalLiteral_in_integerLiteral501);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(on.getText().substring(1), 8));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:164:9: nn= DecimalLiteral
			{
			nn = (Token) input.LT(1);
			match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_integerLiteral519);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:169:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:171:3: ( '#^' s= symbol | '#^' m= mapExpression )
		int alt7 = 2;
		int LA7_0 = input.LA(1);

		if((LA7_0 == 36))
			{
			int LA7_1 = input.LA(2);

			if((LA7_1 == 32))
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
								"169:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
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
							"169:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
							7, 0, input);

			throw nvae;
			}
		switch(alt7)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:171:3: '#^' s= symbol
			{
			match(input, 36, FOLLOW_36_in_metaTag542);
			if(failed) return val;
			pushFollow(FOLLOW_symbol_in_metaTag548);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:172:3: '#^' m= mapExpression
			{
			match(input, 36, FOLLOW_36_in_metaTag554);
			if(failed) return val;
			pushFollow(FOLLOW_mapExpression_in_metaTag560);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:175:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:177:3: (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression )
		int alt8 = 4;
		switch(input.LA(1))
			{
			case Identifier:
			case JavaIdentifier:
			case DotDot:
			{
			alt8 = 1;
			}
			break;
			case 28:
			{
			alt8 = 2;
			}
			break;
			case 32:
			{
			alt8 = 3;
			}
			break;
			case 30:
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
								"175:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );",
								8, 0, input);

				throw nvae;
			}

		switch(alt8)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:177:3: s= symbol
			{
			pushFollow(FOLLOW_symbol_in_objExpression583);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:178:3: le= listExpression
			{
			pushFollow(FOLLOW_listExpression_in_objExpression593);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:179:3: me= mapExpression
			{
			pushFollow(FOLLOW_mapExpression_in_objExpression603);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:180:3: ve= vectorExpression
			{
			pushFollow(FOLLOW_vectorExpression_in_objExpression613);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:183:1: metaExpression returns [Obj val] : m= metaTag e= objExpression ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:184:3: (m= metaTag e= objExpression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:184:3: m= metaTag e= objExpression
		{
		pushFollow(FOLLOW_metaTag_in_metaExpression635);
		m = metaTag();
		_fsp--;
		if(failed) return val;
		pushFollow(FOLLOW_objExpression_in_metaExpression641);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:187:1: fragment member returns [Object val] : ( '.' i= JavaIdentifier | '.' m= method );

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:189:4: ( '.' i= JavaIdentifier | '.' m= method )
		int alt9 = 2;
		int LA9_0 = input.LA(1);

		if((LA9_0 == 37))
			{
			int LA9_1 = input.LA(2);

			if((LA9_1 == JavaIdentifier))
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
								"187:1: fragment member returns [Object val] : ( '.' i= JavaIdentifier | '.' m= method );",
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
							"187:1: fragment member returns [Object val] : ( '.' i= JavaIdentifier | '.' m= method );",
							9, 0, input);

			throw nvae;
			}
		switch(alt9)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:189:4: '.' i= JavaIdentifier
			{
			match(input, 37, FOLLOW_37_in_member661);
			if(failed) return val;
			i = (Token) input.LT(1);
			match(input, JavaIdentifier, FOLLOW_JavaIdentifier_in_member667);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(i.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:190:4: '.' m= method
			{
			match(input, 37, FOLLOW_37_in_member674);
			if(failed) return val;
			pushFollow(FOLLOW_method_in_member680);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:193:1: fragment method returns [Object val] : i= MethodIdentifier (es= args )? ')' ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:195:4: (i= MethodIdentifier (es= args )? ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:195:4: i= MethodIdentifier (es= args )? ')'
		{
		i = (Token) input.LT(1);
		match(input, MethodIdentifier, FOLLOW_MethodIdentifier_in_method705);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:195:28: (es= args )?
		int alt10 = 2;
		int LA10_0 = input.LA(1);

		if(((LA10_0 >= Identifier && LA10_0 <= OctalLiteral) || LA10_0 == 28 || LA10_0 == 30 || LA10_0 == 32 ||
		    (LA10_0 >= 35 && LA10_0 <= 36)))
			{
			alt10 = 1;
			}
		switch(alt10)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: es= args
			{
			pushFollow(FOLLOW_args_in_method711);
			es = args();
			_fsp--;
			if(failed) return val;

			}
			break;

			}

		match(input, 29, FOLLOW_29_in_method714);
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:198:10: fragment args returns [ISeq val] : e1= expression ( Comma e= expression )* ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:202:4: (e1= expression ( Comma e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:202:4: e1= expression ( Comma e= expression )*
		{
		pushFollow(FOLLOW_expression_in_args740);
		e1 = expression();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:202:21: ( Comma e= expression )*
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
					// /Users/rich/dev/clojure/src/jvm/Reader.g:202:22: Comma e= expression
				{
				match(input, Comma, FOLLOW_Comma_in_args744);
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_args750);
				e = expression();
				_fsp--;
				if(failed) return val;
				//*WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED*/

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
// /Users/rich/dev/clojure/src/jvm/Reader.g:205:1: dotExpression returns [Object val] : s= symbol (e= member )+ ;

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
		// /Users/rich/dev/clojure/src/jvm/Reader.g:209:3: (s= symbol (e= member )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:209:3: s= symbol (e= member )+
		{
		pushFollow(FOLLOW_symbol_in_dotExpression779);
		s = symbol();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:209:16: (e= member )+
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
				pushFollow(FOLLOW_member_in_dotExpression785);
				e = member();
				_fsp--;
				if(failed) return val;
				//*WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED*/

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


public static final BitSet FOLLOW_literal_in_expression73 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_expression83 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_keyword_in_expression93 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_expression103 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_expression113 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_expression123 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaExpression_in_expression133 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_dotExpression_in_expression143 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_28_in_listExpression160 = new BitSet(new long[]{0x0000001970007FF0L});
public static final BitSet FOLLOW_expressions_in_listExpression167 = new BitSet(new long[]{0x0000000020000000L});
public static final BitSet FOLLOW_29_in_listExpression169 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_expressions191 = new BitSet(new long[]{0x0000001950007FF2L});
public static final BitSet FOLLOW_30_in_vectorExpression208 = new BitSet(new long[]{0x00000019D0007FF0L});
public static final BitSet FOLLOW_expressions_in_vectorExpression214 = new BitSet(new long[]{0x0000000080000000L});
public static final BitSet FOLLOW_31_in_vectorExpression216 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_32_in_mapExpression236 = new BitSet(new long[]{0x0000001B50007FF0L});
public static final BitSet FOLLOW_expression_in_mapExpression241 = new BitSet(new long[]{0x0000001950007FF0L});
public static final BitSet FOLLOW_expression_in_mapExpression245 = new BitSet(new long[]{0x0000001B50007FF0L});
public static final BitSet FOLLOW_33_in_mapExpression249 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_set_in_symbol266 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_set_in_symbol280 = new BitSet(new long[]{0x0000000400000000L});
public static final BitSet FOLLOW_34_in_symbol286 = new BitSet(new long[]{0x0000000000000030L});
public static final BitSet FOLLOW_set_in_symbol292 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DotDot_in_symbol306 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_35_in_keyword322 = new BitSet(new long[]{0x0000000000000070L});
public static final BitSet FOLLOW_symbol_in_keyword328 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_integerLiteral_in_literal355 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_FloatingPointLiteral_in_literal368 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_CharacterLiteral_in_literal381 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_StringLiteral_in_literal394 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_TrueToken_in_literal403 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_NullToken_in_literal412 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_ratioLiteral_in_literal425 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral448 = new BitSet(new long[]{0x0000000400000000L});
public static final BitSet FOLLOW_34_in_ratioLiteral450 = new BitSet(new long[]{0x0000000000001000L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral456 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_HexLiteral_in_integerLiteral482 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_OctalLiteral_in_integerLiteral501 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_integerLiteral519 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag542 = new BitSet(new long[]{0x0000000000000070L});
public static final BitSet FOLLOW_symbol_in_metaTag548 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag554 = new BitSet(new long[]{0x0000000100000000L});
public static final BitSet FOLLOW_mapExpression_in_metaTag560 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_objExpression583 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_objExpression593 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_objExpression603 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_objExpression613 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaTag_in_metaExpression635 = new BitSet(new long[]{0x0000000150000070L});
public static final BitSet FOLLOW_objExpression_in_metaExpression641 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member661 = new BitSet(new long[]{0x0000000000000020L});
public static final BitSet FOLLOW_JavaIdentifier_in_member667 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member674 = new BitSet(new long[]{0x0000000000008000L});
public static final BitSet FOLLOW_method_in_member680 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_MethodIdentifier_in_method705 = new BitSet(new long[]{0x0000001970007FF0L});
public static final BitSet FOLLOW_args_in_method711 = new BitSet(new long[]{0x0000000020000000L});
public static final BitSet FOLLOW_29_in_method714 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_args740 = new BitSet(new long[]{0x0000000000010002L});
public static final BitSet FOLLOW_Comma_in_args744 = new BitSet(new long[]{0x0000001950007FF0L});
public static final BitSet FOLLOW_expression_in_args750 = new BitSet(new long[]{0x0000000000010002L});
public static final BitSet FOLLOW_symbol_in_dotExpression779 = new BitSet(new long[]{0x0000002000000000L});
public static final BitSet FOLLOW_member_in_dotExpression785 = new BitSet(new long[]{0x0000002000000002L});

}