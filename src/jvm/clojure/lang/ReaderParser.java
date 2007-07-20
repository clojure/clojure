// $ANTLR 3.0 /Users/rich/dev/clojure/src/jvm/Reader.g 2007-07-20 09:22:37

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
	ruleMemo = new HashMap[49 + 1];
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
// /Users/rich/dev/clojure/src/jvm/Reader.g:121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );
public final Object expression() throws RecognitionException{
	Object val = null;
	int expression_StartIndex = input.index();
	Object d = null;

	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 1))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:122:3: (d= dotExpression | e= otherThanDotExpression )
		int alt1 = 2;
		switch(input.LA(1))
			{
			case HexLiteral:
			{
			int LA1_1 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 1, input);

				throw nvae;
				}
			}
			break;
			case OctalLiteral:
			{
			int LA1_2 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 2, input);

				throw nvae;
				}
			}
			break;
			case DecimalLiteral:
			{
			int LA1_3 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 3, input);

				throw nvae;
				}
			}
			break;
			case FloatingPointLiteral:
			{
			int LA1_4 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 4, input);

				throw nvae;
				}
			}
			break;
			case CharacterLiteral:
			{
			int LA1_5 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 5, input);

				throw nvae;
				}
			}
			break;
			case StringLiteral:
			{
			int LA1_6 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 6, input);

				throw nvae;
				}
			}
			break;
			case TrueToken:
			{
			int LA1_7 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 7, input);

				throw nvae;
				}
			}
			break;
			case NullToken:
			{
			int LA1_8 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 8, input);

				throw nvae;
				}
			}
			break;
			case Identifier:
			{
			int LA1_9 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 9, input);

				throw nvae;
				}
			}
			break;
			case NSIdentifier:
			{
			int LA1_10 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 10, input);

				throw nvae;
				}
			}
			break;
			case DotDot:
			{
			int LA1_11 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 11, input);

				throw nvae;
				}
			}
			break;
			case KeywordIdentifier:
			{
			int LA1_12 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 12, input);

				throw nvae;
				}
			}
			break;
			case 29:
			{
			int LA1_13 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 13, input);

				throw nvae;
				}
			}
			break;
			case 31:
			{
			int LA1_14 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 14, input);

				throw nvae;
				}
			}
			break;
			case 33:
			{
			int LA1_15 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 15, input);

				throw nvae;
				}
			}
			break;
			case 36:
			{
			int LA1_16 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 16, input);

				throw nvae;
				}
			}
			break;
			case 38:
			{
			int LA1_17 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 17, input);

				throw nvae;
				}
			}
			break;
			case 39:
			{
			int LA1_18 = input.LA(2);

			if((synpred1()))
				{
				alt1 = 1;
				}
			else if((true))
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 18, input);

				throw nvae;
				}
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
								"121:1: expression returns [Object val] : (d= dotExpression | e= otherThanDotExpression );",
								1, 0, input);

				throw nvae;
			}

		switch(alt1)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:122:3: d= dotExpression
			{
			pushFollow(FOLLOW_dotExpression_in_expression72);
			d = dotExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = d;
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:123:4: e= otherThanDotExpression
			{
			pushFollow(FOLLOW_otherThanDotExpression_in_expression83);
			e = otherThanDotExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = e;
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

// $ANTLR start otherThanDotExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:126:1: fragment otherThanDotExpression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | q= quotedExpression | ct= caretExpression );

public final Object otherThanDotExpression() throws RecognitionException{
	Object val = null;
	int otherThanDotExpression_StartIndex = input.index();
	Object lt = null;

	Symbol s = null;

	Keyword k = null;

	ISeq le = null;

	IPersistentArray ve = null;

	IPersistentMap me = null;

	Obj mx = null;

	Object q = null;

	Object ct = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 2))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:128:4: (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | q= quotedExpression | ct= caretExpression )
		int alt2 = 9;
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
			alt2 = 1;
			}
			break;
			case Identifier:
			case NSIdentifier:
			case DotDot:
			{
			alt2 = 2;
			}
			break;
			case KeywordIdentifier:
			{
			alt2 = 3;
			}
			break;
			case 29:
			{
			alt2 = 4;
			}
			break;
			case 31:
			{
			alt2 = 5;
			}
			break;
			case 33:
			{
			alt2 = 6;
			}
			break;
			case 36:
			{
			alt2 = 7;
			}
			break;
			case 38:
			{
			alt2 = 8;
			}
			break;
			case 39:
			{
			alt2 = 9;
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
								"126:1: fragment otherThanDotExpression returns [Object val] : (lt= literal | s= symbol | k= keyword | le= listExpression | ve= vectorExpression | me= mapExpression | mx= metaExpression | q= quotedExpression | ct= caretExpression );",
								2, 0, input);

				throw nvae;
			}

		switch(alt2)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:128:4: lt= literal
			{
			pushFollow(FOLLOW_literal_in_otherThanDotExpression106);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:129:3: s= symbol
			{
			pushFollow(FOLLOW_symbol_in_otherThanDotExpression116);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:130:3: k= keyword
			{
			pushFollow(FOLLOW_keyword_in_otherThanDotExpression126);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:131:3: le= listExpression
			{
			pushFollow(FOLLOW_listExpression_in_otherThanDotExpression136);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:132:3: ve= vectorExpression
			{
			pushFollow(FOLLOW_vectorExpression_in_otherThanDotExpression146);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:133:3: me= mapExpression
			{
			pushFollow(FOLLOW_mapExpression_in_otherThanDotExpression156);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:134:3: mx= metaExpression
			{
			pushFollow(FOLLOW_metaExpression_in_otherThanDotExpression166);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:135:3: q= quotedExpression
			{
			pushFollow(FOLLOW_quotedExpression_in_otherThanDotExpression176);
			q = quotedExpression();
			_fsp--;
			if(failed) return val;
			if(backtracking == 0)
				{
				val = q;
				}

			}
			break;
			case 9:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:136:3: ct= caretExpression
			{
			pushFollow(FOLLOW_caretExpression_in_otherThanDotExpression186);
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
			memoize(input, 2, otherThanDotExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end otherThanDotExpression

// $ANTLR start listExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:139:1: listExpression returns [ISeq val] : '(' es= expressions ')' ;

public final ISeq listExpression() throws RecognitionException{
	ISeq val = null;
	int listExpression_StartIndex = input.index();
	List es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 3))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:140:4: ( '(' es= expressions ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:140:4: '(' es= expressions ')'
		{
		match(input, 29, FOLLOW_29_in_listExpression203);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_listExpression210);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 30, FOLLOW_30_in_listExpression212);
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
			memoize(input, 3, listExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end listExpression

// $ANTLR start expressions
// /Users/rich/dev/clojure/src/jvm/Reader.g:143:10: fragment expressions returns [List es] : (e= expression )* ;

public final List expressions() throws RecognitionException{
	List es = null;
	int expressions_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 4))
			{
			return es;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:144:4: ( (e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:144:4: (e= expression )*
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:144:6: (e= expression )*
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
					// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: e= expression
				{
				pushFollow(FOLLOW_expression_in_expressions234);
				e = expression();
				_fsp--;
				if(failed) return es;
				//WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED

				}
				break;

				default:
					break loop3;
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
			memoize(input, 4, expressions_StartIndex);
			}
		}
	return es;
}
// $ANTLR end expressions

// $ANTLR start vectorExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:147:1: vectorExpression returns [IPersistentArray val] : '[' es= expressions ']' ;

public final IPersistentArray vectorExpression() throws RecognitionException{
	IPersistentArray val = null;
	int vectorExpression_StartIndex = input.index();
	List es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 5))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:148:4: ( '[' es= expressions ']' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:148:4: '[' es= expressions ']'
		{
		match(input, 31, FOLLOW_31_in_vectorExpression251);
		if(failed) return val;
		pushFollow(FOLLOW_expressions_in_vectorExpression257);
		es = expressions();
		_fsp--;
		if(failed) return val;
		match(input, 32, FOLLOW_32_in_vectorExpression259);
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
			memoize(input, 5, vectorExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end vectorExpression

// $ANTLR start mapExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:151:1: mapExpression returns [IPersistentMap val] : '{' (k= expression v= expression )* '}' ;

public final IPersistentMap mapExpression() throws RecognitionException{
	IPersistentMap val = null;
	int mapExpression_StartIndex = input.index();
	Object k = null;

	Object v = null;


	val = PersistentHashMap.EMPTY;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 6))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:155:4: ( '{' (k= expression v= expression )* '}' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:155:4: '{' (k= expression v= expression )* '}'
		{
		match(input, 33, FOLLOW_33_in_mapExpression279);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:155:8: (k= expression v= expression )*
		loop4:
		do
			{
			int alt4 = 2;
			int LA4_0 = input.LA(1);

			if(((LA4_0 >= Identifier && LA4_0 <= OctalLiteral) || LA4_0 == 29 || LA4_0 == 31 || LA4_0 == 33 ||
			    LA4_0 == 36 || (LA4_0 >= 38 && LA4_0 <= 39)))
				{
				alt4 = 1;
				}


			switch(alt4)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:155:9: k= expression v= expression
				{
				pushFollow(FOLLOW_expression_in_mapExpression284);
				k = expression();
				_fsp--;
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_mapExpression288);
				v = expression();
				_fsp--;
				if(failed) return val;
				//WARNING- HAND MODIFIED!
				val = val.assoc(k, v);
				// END HAND MODIFIED

				}
				break;

				default:
					break loop4;
				}
			} while(true);

		match(input, 34, FOLLOW_34_in_mapExpression292);
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
			memoize(input, 6, mapExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end mapExpression

// $ANTLR start symbol
// /Users/rich/dev/clojure/src/jvm/Reader.g:158:1: symbol returns [Symbol val] : (n= Identifier | n= NSIdentifier | dd= DotDot );

public final Symbol symbol() throws RecognitionException{
	Symbol val = null;
	int symbol_StartIndex = input.index();
	Token n = null;
	Token dd = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 7))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:159:3: (n= Identifier | n= NSIdentifier | dd= DotDot )
		int alt5 = 3;
		switch(input.LA(1))
			{
			case Identifier:
			{
			alt5 = 1;
			}
			break;
			case NSIdentifier:
			{
			alt5 = 2;
			}
			break;
			case DotDot:
			{
			alt5 = 3;
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
								"158:1: symbol returns [Symbol val] : (n= Identifier | n= NSIdentifier | dd= DotDot );",
								5, 0, input);

				throw nvae;
			}

		switch(alt5)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:159:3: n= Identifier
			{
			n = (Token) input.LT(1);
			match(input, Identifier, FOLLOW_Identifier_in_symbol309);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(n.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:160:3: n= NSIdentifier
			{
			n = (Token) input.LT(1);
			match(input, NSIdentifier, FOLLOW_NSIdentifier_in_symbol319);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(n.getText());
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:161:3: dd= DotDot
			{
			dd = (Token) input.LT(1);
			match(input, DotDot, FOLLOW_DotDot_in_symbol329);
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
			memoize(input, 7, symbol_StartIndex);
			}
		}
	return val;
}
// $ANTLR end symbol

// $ANTLR start keyword
// /Users/rich/dev/clojure/src/jvm/Reader.g:164:1: keyword returns [Keyword val] : k= KeywordIdentifier ;

public final Keyword keyword() throws RecognitionException{
	Keyword val = null;
	int keyword_StartIndex = input.index();
	Token k = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 8))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:165:3: (k= KeywordIdentifier )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:165:3: k= KeywordIdentifier
		{
		k = (Token) input.LT(1);
		match(input, KeywordIdentifier, FOLLOW_KeywordIdentifier_in_keyword349);
		if(failed) return val;
		if(backtracking == 0)
			{
			val = new Keyword(new Symbol(k.getText().substring(1)));
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
			memoize(input, 8, keyword_StartIndex);
			}
		}
	return val;
}
// $ANTLR end keyword

// $ANTLR start literal
// /Users/rich/dev/clojure/src/jvm/Reader.g:169:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );

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
		if(backtracking > 0 && alreadyParsedRule(input, 9))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:170:6: (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral )
		int alt6 = 7;
		switch(input.LA(1))
			{
			case HexLiteral:
			case OctalLiteral:
			{
			alt6 = 1;
			}
			break;
			case DecimalLiteral:
			{
			int LA6_2 = input.LA(2);

			if((LA6_2 == 35))
				{
				alt6 = 7;
				}
			else if((LA6_2 == EOF || (LA6_2 >= Identifier && LA6_2 <= OctalLiteral) || LA6_2 == Comma ||
			         (LA6_2 >= 29 && LA6_2 <= 34) || (LA6_2 >= 36 && LA6_2 <= 39)))
				{
				alt6 = 1;
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
								"169:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
								6, 2, input);

				throw nvae;
				}
			}
			break;
			case FloatingPointLiteral:
			{
			alt6 = 2;
			}
			break;
			case CharacterLiteral:
			{
			alt6 = 3;
			}
			break;
			case StringLiteral:
			{
			alt6 = 4;
			}
			break;
			case TrueToken:
			{
			alt6 = 5;
			}
			break;
			case NullToken:
			{
			alt6 = 6;
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
								"169:1: literal returns [Object val] : (i= integerLiteral | fp= FloatingPointLiteral | c= CharacterLiteral | s= StringLiteral | TrueToken | NullToken | r= ratioLiteral );",
								6, 0, input);

				throw nvae;
			}

		switch(alt6)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:170:6: i= integerLiteral
			{
			pushFollow(FOLLOW_integerLiteral_in_literal376);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:171:6: fp= FloatingPointLiteral
			{
			fp = (Token) input.LT(1);
			match(input, FloatingPointLiteral, FOLLOW_FloatingPointLiteral_in_literal389);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(Double.valueOf(fp.getText()));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:172:6: c= CharacterLiteral
			{
			c = (Token) input.LT(1);
			match(input, CharacterLiteral, FOLLOW_CharacterLiteral_in_literal402);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = c.getText().charAt(0);
				}

			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:173:6: s= StringLiteral
			{
			s = (Token) input.LT(1);
			match(input, StringLiteral, FOLLOW_StringLiteral_in_literal415);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = s.getText().substring(1, s.getText().length() - 1);
				}

			}
			break;
			case 5:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:174:6: TrueToken
			{
			match(input, TrueToken, FOLLOW_TrueToken_in_literal424);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Boolean.TRUE;
				}

			}
			break;
			case 6:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:175:6: NullToken
			{
			match(input, NullToken, FOLLOW_NullToken_in_literal433);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = null;
				}

			}
			break;
			case 7:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:176:6: r= ratioLiteral
			{
			pushFollow(FOLLOW_ratioLiteral_in_literal446);
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
			memoize(input, 9, literal_StartIndex);
			}
		}
	return val;
}
// $ANTLR end literal

// $ANTLR start ratioLiteral
// /Users/rich/dev/clojure/src/jvm/Reader.g:179:1: ratioLiteral returns [Num val] : n= DecimalLiteral '/' d= DecimalLiteral ;

public final Num ratioLiteral() throws RecognitionException{
	Num val = null;
	int ratioLiteral_StartIndex = input.index();
	Token n = null;
	Token d = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 10))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:180:4: (n= DecimalLiteral '/' d= DecimalLiteral )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:180:4: n= DecimalLiteral '/' d= DecimalLiteral
		{
		n = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral469);
		if(failed) return val;
		match(input, 35, FOLLOW_35_in_ratioLiteral471);
		if(failed) return val;
		d = (Token) input.LT(1);
		match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_ratioLiteral477);
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
			memoize(input, 10, ratioLiteral_StartIndex);
			}
		}
	return val;
}
// $ANTLR end ratioLiteral

// $ANTLR start integerLiteral
// /Users/rich/dev/clojure/src/jvm/Reader.g:183:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );

public final Num integerLiteral() throws RecognitionException{
	Num val = null;
	int integerLiteral_StartIndex = input.index();
	Token hn = null;
	Token on = null;
	Token nn = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 11))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:184:9: (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral )
		int alt7 = 3;
		switch(input.LA(1))
			{
			case HexLiteral:
			{
			alt7 = 1;
			}
			break;
			case OctalLiteral:
			{
			alt7 = 2;
			}
			break;
			case DecimalLiteral:
			{
			alt7 = 3;
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
								"183:1: integerLiteral returns [Num val] : (hn= HexLiteral | on= OctalLiteral | nn= DecimalLiteral );",
								7, 0, input);

				throw nvae;
			}

		switch(alt7)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:184:9: hn= HexLiteral
			{
			hn = (Token) input.LT(1);
			match(input, HexLiteral, FOLLOW_HexLiteral_in_integerLiteral503);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(hn.getText().substring(2), 16));
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:185:9: on= OctalLiteral
			{
			on = (Token) input.LT(1);
			match(input, OctalLiteral, FOLLOW_OctalLiteral_in_integerLiteral522);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = Num.from(new BigInteger(on.getText().substring(1), 8));
				}

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:186:9: nn= DecimalLiteral
			{
			nn = (Token) input.LT(1);
			match(input, DecimalLiteral, FOLLOW_DecimalLiteral_in_integerLiteral540);
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
			memoize(input, 11, integerLiteral_StartIndex);
			}
		}
	return val;
}
// $ANTLR end integerLiteral

// $ANTLR start metaTag
// /Users/rich/dev/clojure/src/jvm/Reader.g:191:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );

public final IPersistentMap metaTag() throws RecognitionException{
	IPersistentMap val = null;
	int metaTag_StartIndex = input.index();
	Symbol s = null;

	IPersistentMap m = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 12))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:193:3: ( '#^' s= symbol | '#^' m= mapExpression )
		int alt8 = 2;
		int LA8_0 = input.LA(1);

		if((LA8_0 == 36))
			{
			int LA8_1 = input.LA(2);

			if(((LA8_1 >= Identifier && LA8_1 <= DotDot)))
				{
				alt8 = 1;
				}
			else if((LA8_1 == 33))
				{
				alt8 = 2;
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
								"191:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
								8, 1, input);

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
							"191:1: fragment metaTag returns [IPersistentMap val] : ( '#^' s= symbol | '#^' m= mapExpression );",
							8, 0, input);

			throw nvae;
			}
		switch(alt8)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:193:3: '#^' s= symbol
			{
			match(input, 36, FOLLOW_36_in_metaTag563);
			if(failed) return val;
			pushFollow(FOLLOW_symbol_in_metaTag569);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:194:3: '#^' m= mapExpression
			{
			match(input, 36, FOLLOW_36_in_metaTag575);
			if(failed) return val;
			pushFollow(FOLLOW_mapExpression_in_metaTag581);
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
			memoize(input, 12, metaTag_StartIndex);
			}
		}
	return val;
}
// $ANTLR end metaTag

// $ANTLR start objExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:197:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );

public final Obj objExpression() throws RecognitionException{
	Obj val = null;
	int objExpression_StartIndex = input.index();
	Symbol s = null;

	ISeq le = null;

	IPersistentMap me = null;

	IPersistentArray ve = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 13))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:199:3: (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression )
		int alt9 = 4;
		switch(input.LA(1))
			{
			case Identifier:
			case NSIdentifier:
			case DotDot:
			{
			alt9 = 1;
			}
			break;
			case 29:
			{
			alt9 = 2;
			}
			break;
			case 33:
			{
			alt9 = 3;
			}
			break;
			case 31:
			{
			alt9 = 4;
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
								"197:1: fragment objExpression returns [Obj val] : (s= symbol | le= listExpression | me= mapExpression | ve= vectorExpression );",
								9, 0, input);

				throw nvae;
			}

		switch(alt9)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:199:3: s= symbol
			{
			pushFollow(FOLLOW_symbol_in_objExpression604);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:200:3: le= listExpression
			{
			pushFollow(FOLLOW_listExpression_in_objExpression614);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:201:3: me= mapExpression
			{
			pushFollow(FOLLOW_mapExpression_in_objExpression624);
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
				// /Users/rich/dev/clojure/src/jvm/Reader.g:202:3: ve= vectorExpression
			{
			pushFollow(FOLLOW_vectorExpression_in_objExpression634);
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
			memoize(input, 13, objExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end objExpression

// $ANTLR start metaExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:205:1: metaExpression returns [Obj val] : m= metaTag e= objExpression ;

public final Obj metaExpression() throws RecognitionException{
	Obj val = null;
	int metaExpression_StartIndex = input.index();
	IPersistentMap m = null;

	Obj e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 14))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:206:3: (m= metaTag e= objExpression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:206:3: m= metaTag e= objExpression
		{
		pushFollow(FOLLOW_metaTag_in_metaExpression656);
		m = metaTag();
		_fsp--;
		if(failed) return val;
		pushFollow(FOLLOW_objExpression_in_metaExpression662);
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
			memoize(input, 14, metaExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end metaExpression

// $ANTLR start member
// /Users/rich/dev/clojure/src/jvm/Reader.g:209:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );

public final Object member() throws RecognitionException{
	Object val = null;
	int member_StartIndex = input.index();
	Token i = null;
	Object m = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 15))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:211:4: ( '.' i= Identifier | '.' m= method )
		int alt10 = 2;
		int LA10_0 = input.LA(1);

		if((LA10_0 == 37))
			{
			int LA10_1 = input.LA(2);

			if((LA10_1 == Identifier))
				{
				alt10 = 1;
				}
			else if((LA10_1 == MethodIdentifier))
				{
				alt10 = 2;
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
								"209:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );",
								10, 1, input);

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
							"209:1: fragment member returns [Object val] : ( '.' i= Identifier | '.' m= method );", 10,
							0, input);

			throw nvae;
			}
		switch(alt10)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:211:4: '.' i= Identifier
			{
			match(input, 37, FOLLOW_37_in_member682);
			if(failed) return val;
			i = (Token) input.LT(1);
			match(input, Identifier, FOLLOW_Identifier_in_member688);
			if(failed) return val;
			if(backtracking == 0)
				{
				val = new Symbol(i.getText());
				}

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:212:4: '.' m= method
			{
			match(input, 37, FOLLOW_37_in_member695);
			if(failed) return val;
			pushFollow(FOLLOW_method_in_member701);
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
			memoize(input, 15, member_StartIndex);
			}
		}
	return val;
}
// $ANTLR end member

// $ANTLR start method
// /Users/rich/dev/clojure/src/jvm/Reader.g:215:1: fragment method returns [Object val] : i= MethodIdentifier (es= args )? ')' ;

public final Object method() throws RecognitionException{
	Object val = null;
	int method_StartIndex = input.index();
	Token i = null;
	ISeq es = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 16))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:217:4: (i= MethodIdentifier (es= args )? ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:217:4: i= MethodIdentifier (es= args )? ')'
		{
		i = (Token) input.LT(1);
		match(input, MethodIdentifier, FOLLOW_MethodIdentifier_in_method726);
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:217:28: (es= args )?
		int alt11 = 2;
		int LA11_0 = input.LA(1);

		if(((LA11_0 >= Identifier && LA11_0 <= OctalLiteral) || LA11_0 == 29 || LA11_0 == 31 || LA11_0 == 33 ||
		    LA11_0 == 36 || (LA11_0 >= 38 && LA11_0 <= 39)))
			{
			alt11 = 1;
			}
		switch(alt11)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: es= args
			{
			pushFollow(FOLLOW_args_in_method732);
			es = args();
			_fsp--;
			if(failed) return val;

			}
			break;

			}

		match(input, 30, FOLLOW_30_in_method735);
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
			memoize(input, 16, method_StartIndex);
			}
		}
	return val;
}
// $ANTLR end method

// $ANTLR start args
// /Users/rich/dev/clojure/src/jvm/Reader.g:220:10: fragment args returns [ISeq val] : e1= expression ( Comma e= expression )* ;

public final ISeq args() throws RecognitionException{
	ISeq val = null;
	int args_StartIndex = input.index();
	Object e1 = null;

	Object e = null;


	List es = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 17))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:224:4: (e1= expression ( Comma e= expression )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:224:4: e1= expression ( Comma e= expression )*
		{
		pushFollow(FOLLOW_expression_in_args761);
		e1 = expression();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:224:21: ( Comma e= expression )*
		loop12:
		do
			{
			int alt12 = 2;
			int LA12_0 = input.LA(1);

			if((LA12_0 == Comma))
				{
				alt12 = 1;
				}


			switch(alt12)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:224:22: Comma e= expression
				{
				match(input, Comma, FOLLOW_Comma_in_args765);
				if(failed) return val;
				pushFollow(FOLLOW_expression_in_args771);
				e = expression();
				_fsp--;
				if(failed) return val;
				//WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED

				}
				break;

				default:
					break loop12;
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
			memoize(input, 17, args_StartIndex);
			}
		}
	return val;
}
// $ANTLR end args

// $ANTLR start dotExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:227:1: dotExpression returns [Object val] : s= otherThanDotExpression (e= member )+ ;

public final Object dotExpression() throws RecognitionException{
	Object val = null;
	int dotExpression_StartIndex = input.index();
	Object s = null;

	Object e = null;


	List es = null;

	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 18))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:231:3: (s= otherThanDotExpression (e= member )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:231:3: s= otherThanDotExpression (e= member )+
		{
		pushFollow(FOLLOW_otherThanDotExpression_in_dotExpression800);
		s = otherThanDotExpression();
		_fsp--;
		if(failed) return val;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:231:32: (e= member )+
		int cnt13 = 0;
		loop13:
		do
			{
			int alt13 = 2;
			int LA13_0 = input.LA(1);

			if((LA13_0 == 37))
				{
				int LA13_2 = input.LA(2);

				if((synpred29()))
					{
					alt13 = 1;
					}


				}


			switch(alt13)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:0:0: e= member
				{
				pushFollow(FOLLOW_member_in_dotExpression806);
				e = member();
				_fsp--;
				if(failed) return val;
				//WARNING- HAND MODIFIED!
				if(es == null) es = new ArrayList();
				es.add(e);
				// END HAND MODIFIED

				}
				break;

				default:
					if(cnt13 >= 1) break loop13;
					if(backtracking > 0)
						{
						failed = true;
						return val;
						}
					EarlyExitException eee =
							new EarlyExitException(13, input);
					throw eee;
				}
			cnt13++;
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
			memoize(input, 18, dotExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end dotExpression

// $ANTLR start quotedExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:234:1: quotedExpression returns [Object val] : '\\'' e= expression ;

public final Object quotedExpression() throws RecognitionException{
	Object val = null;
	int quotedExpression_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 19))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:235:3: ( '\\'' e= expression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:235:3: '\\'' e= expression
		{
		match(input, 38, FOLLOW_38_in_quotedExpression824);
		if(failed) return val;
		pushFollow(FOLLOW_expression_in_quotedExpression830);
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
			memoize(input, 19, quotedExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end quotedExpression

// $ANTLR start caretExpression
// /Users/rich/dev/clojure/src/jvm/Reader.g:238:1: caretExpression returns [Object val] : '^' e= expression ;

public final Object caretExpression() throws RecognitionException{
	Object val = null;
	int caretExpression_StartIndex = input.index();
	Object e = null;


	try
		{
		if(backtracking > 0 && alreadyParsedRule(input, 20))
			{
			return val;
			}
		// /Users/rich/dev/clojure/src/jvm/Reader.g:239:3: ( '^' e= expression )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:239:3: '^' e= expression
		{
		match(input, 39, FOLLOW_39_in_caretExpression848);
		if(failed) return val;
		pushFollow(FOLLOW_expression_in_caretExpression854);
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
			memoize(input, 20, caretExpression_StartIndex);
			}
		}
	return val;
}
// $ANTLR end caretExpression

// $ANTLR start synpred1

public final void synpred1_fragment() throws RecognitionException{
	// /Users/rich/dev/clojure/src/jvm/Reader.g:122:3: ( dotExpression )
	// /Users/rich/dev/clojure/src/jvm/Reader.g:122:3: dotExpression
	{
	pushFollow(FOLLOW_dotExpression_in_synpred172);
	dotExpression();
	_fsp--;
	if(failed) return;

	}
}
// $ANTLR end synpred1

// $ANTLR start synpred29

public final void synpred29_fragment() throws RecognitionException{
	// /Users/rich/dev/clojure/src/jvm/Reader.g:231:34: ( member )
	// /Users/rich/dev/clojure/src/jvm/Reader.g:231:34: member
	{
	pushFollow(FOLLOW_member_in_synpred29806);
	member();
	_fsp--;
	if(failed) return;

	}
}
// $ANTLR end synpred29

public final boolean synpred29(){
	backtracking++;
	int start = input.mark();
	try
		{
		synpred29_fragment(); // can never throw exception
		}
	catch(RecognitionException re)
		{
		System.err.println("impossible: " + re);
		}
	boolean success = !failed;
	input.rewind(start);
	backtracking--;
	failed = false;
	return success;
}

public final boolean synpred1(){
	backtracking++;
	int start = input.mark();
	try
		{
		synpred1_fragment(); // can never throw exception
		}
	catch(RecognitionException re)
		{
		System.err.println("impossible: " + re);
		}
	boolean success = !failed;
	input.rewind(start);
	backtracking--;
	failed = false;
	return success;
}


public static final BitSet FOLLOW_dotExpression_in_expression72 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_otherThanDotExpression_in_expression83 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_literal_in_otherThanDotExpression106 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_otherThanDotExpression116 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_keyword_in_otherThanDotExpression126 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_otherThanDotExpression136 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_otherThanDotExpression146 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_otherThanDotExpression156 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaExpression_in_otherThanDotExpression166 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_quotedExpression_in_otherThanDotExpression176 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_caretExpression_in_otherThanDotExpression186 =
		new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_29_in_listExpression203 = new BitSet(new long[]{0x000000D2E000FFF0L});
public static final BitSet FOLLOW_expressions_in_listExpression210 = new BitSet(new long[]{0x0000000040000000L});
public static final BitSet FOLLOW_30_in_listExpression212 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_expressions234 = new BitSet(new long[]{0x000000D2A000FFF2L});
public static final BitSet FOLLOW_31_in_vectorExpression251 = new BitSet(new long[]{0x000000D3A000FFF0L});
public static final BitSet FOLLOW_expressions_in_vectorExpression257 = new BitSet(new long[]{0x0000000100000000L});
public static final BitSet FOLLOW_32_in_vectorExpression259 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_33_in_mapExpression279 = new BitSet(new long[]{0x000000D6A000FFF0L});
public static final BitSet FOLLOW_expression_in_mapExpression284 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_mapExpression288 = new BitSet(new long[]{0x000000D6A000FFF0L});
public static final BitSet FOLLOW_34_in_mapExpression292 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_Identifier_in_symbol309 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_NSIdentifier_in_symbol319 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DotDot_in_symbol329 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_KeywordIdentifier_in_keyword349 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_integerLiteral_in_literal376 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_FloatingPointLiteral_in_literal389 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_CharacterLiteral_in_literal402 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_StringLiteral_in_literal415 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_TrueToken_in_literal424 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_NullToken_in_literal433 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_ratioLiteral_in_literal446 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral469 = new BitSet(new long[]{0x0000000800000000L});
public static final BitSet FOLLOW_35_in_ratioLiteral471 = new BitSet(new long[]{0x0000000000002000L});
public static final BitSet FOLLOW_DecimalLiteral_in_ratioLiteral477 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_HexLiteral_in_integerLiteral503 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_OctalLiteral_in_integerLiteral522 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_DecimalLiteral_in_integerLiteral540 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag563 = new BitSet(new long[]{0x0000000000000070L});
public static final BitSet FOLLOW_symbol_in_metaTag569 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_36_in_metaTag575 = new BitSet(new long[]{0x0000000200000000L});
public static final BitSet FOLLOW_mapExpression_in_metaTag581 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_symbol_in_objExpression604 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_listExpression_in_objExpression614 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_mapExpression_in_objExpression624 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_vectorExpression_in_objExpression634 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_metaTag_in_metaExpression656 = new BitSet(new long[]{0x00000002A0000070L});
public static final BitSet FOLLOW_objExpression_in_metaExpression662 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member682 = new BitSet(new long[]{0x0000000000000010L});
public static final BitSet FOLLOW_Identifier_in_member688 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_37_in_member695 = new BitSet(new long[]{0x0000000000010000L});
public static final BitSet FOLLOW_method_in_member701 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_MethodIdentifier_in_method726 = new BitSet(new long[]{0x000000D2E000FFF0L});
public static final BitSet FOLLOW_args_in_method732 = new BitSet(new long[]{0x0000000040000000L});
public static final BitSet FOLLOW_30_in_method735 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_expression_in_args761 = new BitSet(new long[]{0x0000000000020002L});
public static final BitSet FOLLOW_Comma_in_args765 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_args771 = new BitSet(new long[]{0x0000000000020002L});
public static final BitSet FOLLOW_otherThanDotExpression_in_dotExpression800 =
		new BitSet(new long[]{0x0000002000000000L});
public static final BitSet FOLLOW_member_in_dotExpression806 = new BitSet(new long[]{0x0000002000000002L});
public static final BitSet FOLLOW_38_in_quotedExpression824 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_quotedExpression830 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_39_in_caretExpression848 = new BitSet(new long[]{0x000000D2A000FFF0L});
public static final BitSet FOLLOW_expression_in_caretExpression854 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_dotExpression_in_synpred172 = new BitSet(new long[]{0x0000000000000002L});
public static final BitSet FOLLOW_member_in_synpred29806 = new BitSet(new long[]{0x0000000000000002L});

}