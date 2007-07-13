// $ANTLR 3.0 /Users/rich/dev/clojure/src/jvm/Reader.g 2007-07-13 12:56:20


package clojure.lang;


import org.antlr.runtime.*;

import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class ReaderLexer extends Lexer{
public static final int TrueToken = 10;
public static final int T29 = 29;
public static final int Exponent = 18;
public static final int OctalLiteral = 14;
public static final int T33 = 33;
public static final int Identifier = 4;
public static final int HexDigit = 17;
public static final int T36 = 36;
public static final int WS = 25;
public static final int CharacterLiteral = 8;
public static final int MethodIdentifier = 15;
public static final int T28 = 28;
public static final int T35 = 35;
public static final int COMMENT = 26;
public static final int StringLiteral = 9;
public static final int LINE_COMMENT = 27;
public static final int DotDot = 6;
public static final int T34 = 34;
public static final int JavaIDDigit = 24;
public static final int Letter = 23;
public static final int UnicodeEscape = 21;
public static final int Comma = 16;
public static final int HexLiteral = 13;
public static final int T37 = 37;
public static final int EscapeSequence = 20;
public static final int NullToken = 11;
public static final int EOF = -1;
public static final int DecimalLiteral = 12;
public static final int T32 = 32;
public static final int Tokens = 38;
public static final int T31 = 31;
public static final int OctalEscape = 22;
public static final int FloatingPointLiteral = 7;
public static final int T30 = 30;
public static final int FloatTypeSuffix = 19;
public static final int JavaIdentifier = 5;

RecognitionException rex = null;

public void recover(RecognitionException re){
	super.recover(re);
	if(rex == null)
		rex = re;
}

public void reportError(RecognitionException e){
	if(rex == null)
		rex = e;
}

public ReaderLexer(){
	;
}

public ReaderLexer(CharStream input){
	super(input);
}

public String getGrammarFileName(){
	return "/Users/rich/dev/clojure/src/jvm/Reader.g";
}

// $ANTLR start T28
public final void mT28() throws RecognitionException{
	try
		{
		int _type = T28;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:25:7: ( '(' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:25:7: '('
		{
		match('(');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T28

// $ANTLR start T29

public final void mT29() throws RecognitionException{
	try
		{
		int _type = T29;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:26:7: ( ')' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:26:7: ')'
		{
		match(')');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T29

// $ANTLR start T30

public final void mT30() throws RecognitionException{
	try
		{
		int _type = T30;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:27:7: ( '[' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:27:7: '['
		{
		match('[');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T30

// $ANTLR start T31

public final void mT31() throws RecognitionException{
	try
		{
		int _type = T31;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:28:7: ( ']' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:28:7: ']'
		{
		match(']');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T31

// $ANTLR start T32

public final void mT32() throws RecognitionException{
	try
		{
		int _type = T32;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:29:7: ( '{' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:29:7: '{'
		{
		match('{');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T32

// $ANTLR start T33

public final void mT33() throws RecognitionException{
	try
		{
		int _type = T33;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:30:7: ( '}' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:30:7: '}'
		{
		match('}');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T33

// $ANTLR start T34

public final void mT34() throws RecognitionException{
	try
		{
		int _type = T34;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:31:7: ( '/' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:31:7: '/'
		{
		match('/');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T34

// $ANTLR start T35

public final void mT35() throws RecognitionException{
	try
		{
		int _type = T35;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:32:7: ( ':' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:32:7: ':'
		{
		match(':');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T35

// $ANTLR start T36

public final void mT36() throws RecognitionException{
	try
		{
		int _type = T36;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:33:7: ( '#^' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:33:7: '#^'
		{
		match("#^");


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T36

// $ANTLR start T37

public final void mT37() throws RecognitionException{
	try
		{
		int _type = T37;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:34:7: ( '.' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:34:7: '.'
		{
		match('.');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end T37

// $ANTLR start Comma

public final void mComma() throws RecognitionException{
	try
		{
		int _type = Comma;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:212:10: ( ',' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:212:10: ','
		{
		match(',');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end Comma

// $ANTLR start TrueToken

public final void mTrueToken() throws RecognitionException{
	try
		{
		int _type = TrueToken;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:213:16: ( 'true' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:213:16: 'true'
		{
		match("true");


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end TrueToken

// $ANTLR start NullToken

public final void mNullToken() throws RecognitionException{
	try
		{
		int _type = NullToken;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:215:16: ( 'null' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:215:16: 'null'
		{
		match("null");


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end NullToken

// $ANTLR start DotDot

public final void mDotDot() throws RecognitionException{
	try
		{
		int _type = DotDot;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:217:10: ( '..' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:217:10: '..'
		{
		match("..");


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end DotDot

// $ANTLR start HexLiteral

public final void mHexLiteral() throws RecognitionException{
	try
		{
		int _type = HexLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:14: ( '0' ( 'x' | 'X' ) ( HexDigit )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:14: '0' ( 'x' | 'X' ) ( HexDigit )+
		{
		match('0');
		if(input.LA(1) == 'X' || input.LA(1) == 'x')
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}

		// /Users/rich/dev/clojure/src/jvm/Reader.g:219:28: ( HexDigit )+
		int cnt1 = 0;
		loop1:
		do
			{
			int alt1 = 2;
			int LA1_0 = input.LA(1);

			if(((LA1_0 >= '0' && LA1_0 <= '9') || (LA1_0 >= 'A' && LA1_0 <= 'F') || (LA1_0 >= 'a' && LA1_0 <= 'f')))
				{
				alt1 = 1;
				}


			switch(alt1)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:219:28: HexDigit
				{
				mHexDigit();

				}
				break;

				default:
					if(cnt1 >= 1) break loop1;
					EarlyExitException eee =
							new EarlyExitException(1, input);
					throw eee;
				}
			cnt1++;
			} while(true);


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end HexLiteral

// $ANTLR start DecimalLiteral

public final void mDecimalLiteral() throws RecognitionException{
	try
		{
		int _type = DecimalLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:221:18: ( ( '0' | '1' .. '9' ( '0' .. '9' )* ) )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:221:18: ( '0' | '1' .. '9' ( '0' .. '9' )* )
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:221:18: ( '0' | '1' .. '9' ( '0' .. '9' )* )
		int alt3 = 2;
		int LA3_0 = input.LA(1);

		if((LA3_0 == '0'))
			{
			alt3 = 1;
			}
		else if(((LA3_0 >= '1' && LA3_0 <= '9')))
			{
			alt3 = 2;
			}
		else
			{
			NoViableAltException nvae =
					new NoViableAltException("221:18: ( '0' | '1' .. '9' ( '0' .. '9' )* )", 3, 0, input);

			throw nvae;
			}
		switch(alt3)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:221:19: '0'
			{
			match('0');

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:221:25: '1' .. '9' ( '0' .. '9' )*
			{
			matchRange('1', '9');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:221:34: ( '0' .. '9' )*
			loop2:
			do
				{
				int alt2 = 2;
				int LA2_0 = input.LA(1);

				if(((LA2_0 >= '0' && LA2_0 <= '9')))
					{
					alt2 = 1;
					}


				switch(alt2)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:221:34: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						break loop2;
					}
				} while(true);


			}
			break;

			}


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end DecimalLiteral

// $ANTLR start OctalLiteral

public final void mOctalLiteral() throws RecognitionException{
	try
		{
		int _type = OctalLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:223:16: ( '0' ( '0' .. '7' )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:223:16: '0' ( '0' .. '7' )+
		{
		match('0');
		// /Users/rich/dev/clojure/src/jvm/Reader.g:223:20: ( '0' .. '7' )+
		int cnt4 = 0;
		loop4:
		do
			{
			int alt4 = 2;
			int LA4_0 = input.LA(1);

			if(((LA4_0 >= '0' && LA4_0 <= '7')))
				{
				alt4 = 1;
				}


			switch(alt4)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:223:21: '0' .. '7'
				{
				matchRange('0', '7');

				}
				break;

				default:
					if(cnt4 >= 1) break loop4;
					EarlyExitException eee =
							new EarlyExitException(4, input);
					throw eee;
				}
			cnt4++;
			} while(true);


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end OctalLiteral

// $ANTLR start HexDigit

public final void mHexDigit() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:226:12: ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:226:12: ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
		{
		if((input.LA(1) >= '0' && input.LA(1) <= '9') || (input.LA(1) >= 'A' && input.LA(1) <= 'F') ||
		   (input.LA(1) >= 'a' && input.LA(1) <= 'f'))
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}


		}

		}
	finally
		{
		}
}
// $ANTLR end HexDigit

// $ANTLR start FloatingPointLiteral

public final void mFloatingPointLiteral() throws RecognitionException{
	try
		{
		int _type = FloatingPointLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:229:9: ( ( '0' .. '9' )+ '.' ( '0' .. '9' )* ( Exponent )? ( FloatTypeSuffix )? | '.' ( '0' .. '9' )+ ( Exponent )? ( FloatTypeSuffix )? | ( '0' .. '9' )+ Exponent ( FloatTypeSuffix )? | ( '0' .. '9' )+ ( Exponent )? FloatTypeSuffix )
		int alt16 = 4;
		alt16 = dfa16.predict(input);
		switch(alt16)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:229:9: ( '0' .. '9' )+ '.' ( '0' .. '9' )* ( Exponent )? ( FloatTypeSuffix )?
			{
			// /Users/rich/dev/clojure/src/jvm/Reader.g:229:9: ( '0' .. '9' )+
			int cnt5 = 0;
			loop5:
			do
				{
				int alt5 = 2;
				int LA5_0 = input.LA(1);

				if(((LA5_0 >= '0' && LA5_0 <= '9')))
					{
					alt5 = 1;
					}


				switch(alt5)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:229:10: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						if(cnt5 >= 1) break loop5;
						EarlyExitException eee =
								new EarlyExitException(5, input);
						throw eee;
					}
				cnt5++;
				} while(true);

			match('.');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:229:25: ( '0' .. '9' )*
			loop6:
			do
				{
				int alt6 = 2;
				int LA6_0 = input.LA(1);

				if(((LA6_0 >= '0' && LA6_0 <= '9')))
					{
					alt6 = 1;
					}


				switch(alt6)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:229:26: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						break loop6;
					}
				} while(true);

			// /Users/rich/dev/clojure/src/jvm/Reader.g:229:37: ( Exponent )?
			int alt7 = 2;
			int LA7_0 = input.LA(1);

			if((LA7_0 == 'E' || LA7_0 == 'e'))
				{
				alt7 = 1;
				}
			switch(alt7)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:229:37: Exponent
				{
				mExponent();

				}
				break;

				}

			// /Users/rich/dev/clojure/src/jvm/Reader.g:229:47: ( FloatTypeSuffix )?
			int alt8 = 2;
			int LA8_0 = input.LA(1);

			if((LA8_0 == 'D' || LA8_0 == 'F' || LA8_0 == 'd' || LA8_0 == 'f'))
				{
				alt8 = 1;
				}
			switch(alt8)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:229:47: FloatTypeSuffix
				{
				mFloatTypeSuffix();

				}
				break;

				}


			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:230:9: '.' ( '0' .. '9' )+ ( Exponent )? ( FloatTypeSuffix )?
			{
			match('.');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:230:13: ( '0' .. '9' )+
			int cnt9 = 0;
			loop9:
			do
				{
				int alt9 = 2;
				int LA9_0 = input.LA(1);

				if(((LA9_0 >= '0' && LA9_0 <= '9')))
					{
					alt9 = 1;
					}


				switch(alt9)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:230:14: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						if(cnt9 >= 1) break loop9;
						EarlyExitException eee =
								new EarlyExitException(9, input);
						throw eee;
					}
				cnt9++;
				} while(true);

			// /Users/rich/dev/clojure/src/jvm/Reader.g:230:25: ( Exponent )?
			int alt10 = 2;
			int LA10_0 = input.LA(1);

			if((LA10_0 == 'E' || LA10_0 == 'e'))
				{
				alt10 = 1;
				}
			switch(alt10)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:230:25: Exponent
				{
				mExponent();

				}
				break;

				}

			// /Users/rich/dev/clojure/src/jvm/Reader.g:230:35: ( FloatTypeSuffix )?
			int alt11 = 2;
			int LA11_0 = input.LA(1);

			if((LA11_0 == 'D' || LA11_0 == 'F' || LA11_0 == 'd' || LA11_0 == 'f'))
				{
				alt11 = 1;
				}
			switch(alt11)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:230:35: FloatTypeSuffix
				{
				mFloatTypeSuffix();

				}
				break;

				}


			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:231:9: ( '0' .. '9' )+ Exponent ( FloatTypeSuffix )?
			{
			// /Users/rich/dev/clojure/src/jvm/Reader.g:231:9: ( '0' .. '9' )+
			int cnt12 = 0;
			loop12:
			do
				{
				int alt12 = 2;
				int LA12_0 = input.LA(1);

				if(((LA12_0 >= '0' && LA12_0 <= '9')))
					{
					alt12 = 1;
					}


				switch(alt12)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:231:10: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						if(cnt12 >= 1) break loop12;
						EarlyExitException eee =
								new EarlyExitException(12, input);
						throw eee;
					}
				cnt12++;
				} while(true);

			mExponent();
			// /Users/rich/dev/clojure/src/jvm/Reader.g:231:30: ( FloatTypeSuffix )?
			int alt13 = 2;
			int LA13_0 = input.LA(1);

			if((LA13_0 == 'D' || LA13_0 == 'F' || LA13_0 == 'd' || LA13_0 == 'f'))
				{
				alt13 = 1;
				}
			switch(alt13)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:231:30: FloatTypeSuffix
				{
				mFloatTypeSuffix();

				}
				break;

				}


			}
			break;
			case 4:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:232:9: ( '0' .. '9' )+ ( Exponent )? FloatTypeSuffix
			{
			// /Users/rich/dev/clojure/src/jvm/Reader.g:232:9: ( '0' .. '9' )+
			int cnt14 = 0;
			loop14:
			do
				{
				int alt14 = 2;
				int LA14_0 = input.LA(1);

				if(((LA14_0 >= '0' && LA14_0 <= '9')))
					{
					alt14 = 1;
					}


				switch(alt14)
					{
					case 1:
						// /Users/rich/dev/clojure/src/jvm/Reader.g:232:10: '0' .. '9'
					{
					matchRange('0', '9');

					}
					break;

					default:
						if(cnt14 >= 1) break loop14;
						EarlyExitException eee =
								new EarlyExitException(14, input);
						throw eee;
					}
				cnt14++;
				} while(true);

			// /Users/rich/dev/clojure/src/jvm/Reader.g:232:21: ( Exponent )?
			int alt15 = 2;
			int LA15_0 = input.LA(1);

			if((LA15_0 == 'E' || LA15_0 == 'e'))
				{
				alt15 = 1;
				}
			switch(alt15)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:232:21: Exponent
				{
				mExponent();

				}
				break;

				}

			mFloatTypeSuffix();

			}
			break;

			}
		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end FloatingPointLiteral

// $ANTLR start Exponent

public final void mExponent() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:236:12: ( ( 'e' | 'E' ) ( '+' | '-' )? ( '0' .. '9' )+ )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:236:12: ( 'e' | 'E' ) ( '+' | '-' )? ( '0' .. '9' )+
		{
		if(input.LA(1) == 'E' || input.LA(1) == 'e')
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}

		// /Users/rich/dev/clojure/src/jvm/Reader.g:236:22: ( '+' | '-' )?
		int alt17 = 2;
		int LA17_0 = input.LA(1);

		if((LA17_0 == '+' || LA17_0 == '-'))
			{
			alt17 = 1;
			}
		switch(alt17)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:
			{
			if(input.LA(1) == '+' || input.LA(1) == '-')
				{
				input.consume();

				}
			else
				{
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recover(mse);
				throw mse;
				}


			}
			break;

			}

		// /Users/rich/dev/clojure/src/jvm/Reader.g:236:33: ( '0' .. '9' )+
		int cnt18 = 0;
		loop18:
		do
			{
			int alt18 = 2;
			int LA18_0 = input.LA(1);

			if(((LA18_0 >= '0' && LA18_0 <= '9')))
				{
				alt18 = 1;
				}


			switch(alt18)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:236:34: '0' .. '9'
				{
				matchRange('0', '9');

				}
				break;

				default:
					if(cnt18 >= 1) break loop18;
					EarlyExitException eee =
							new EarlyExitException(18, input);
					throw eee;
				}
			cnt18++;
			} while(true);


		}

		}
	finally
		{
		}
}
// $ANTLR end Exponent

// $ANTLR start FloatTypeSuffix

public final void mFloatTypeSuffix() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:239:19: ( ( 'f' | 'F' | 'd' | 'D' ) )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:239:19: ( 'f' | 'F' | 'd' | 'D' )
		{
		if(input.LA(1) == 'D' || input.LA(1) == 'F' || input.LA(1) == 'd' || input.LA(1) == 'f')
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}


		}

		}
	finally
		{
		}
}
// $ANTLR end FloatTypeSuffix

// $ANTLR start CharacterLiteral

public final void mCharacterLiteral() throws RecognitionException{
	try
		{
		int _type = CharacterLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:242:9: ( '\\\\' ( EscapeSequence | ~ ( '\\\\' ) ) )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:242:9: '\\\\' ( EscapeSequence | ~ ( '\\\\' ) )
		{
		match('\\');
		// /Users/rich/dev/clojure/src/jvm/Reader.g:242:14: ( EscapeSequence | ~ ( '\\\\' ) )
		int alt19 = 2;
		int LA19_0 = input.LA(1);

		if((LA19_0 == '\\'))
			{
			alt19 = 1;
			}
		else if(((LA19_0 >= '\u0000' && LA19_0 <= '[') || (LA19_0 >= ']' && LA19_0 <= '\uFFFE')))
			{
			alt19 = 2;
			}
		else
			{
			NoViableAltException nvae =
					new NoViableAltException("242:14: ( EscapeSequence | ~ ( '\\\\' ) )", 19, 0, input);

			throw nvae;
			}
		switch(alt19)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:242:16: EscapeSequence
			{
			mEscapeSequence();

			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:242:33: ~ ( '\\\\' )
			{
			if((input.LA(1) >= '\u0000' && input.LA(1) <= '[') || (input.LA(1) >= ']' && input.LA(1) <= '\uFFFE'))
				{
				input.consume();

				}
			else
				{
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recover(mse);
				throw mse;
				}


			}
			break;

			}


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end CharacterLiteral

// $ANTLR start StringLiteral

public final void mStringLiteral() throws RecognitionException{
	try
		{
		int _type = StringLiteral;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:246:8: ( '\"' ( EscapeSequence | ~ ( '\\\\' | '\"' ) )* '\"' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:246:8: '\"' ( EscapeSequence | ~ ( '\\\\' | '\"' ) )* '\"'
		{
		match('\"');
		// /Users/rich/dev/clojure/src/jvm/Reader.g:246:12: ( EscapeSequence | ~ ( '\\\\' | '\"' ) )*
		loop20:
		do
			{
			int alt20 = 3;
			int LA20_0 = input.LA(1);

			if((LA20_0 == '\\'))
				{
				alt20 = 1;
				}
			else if(((LA20_0 >= '\u0000' && LA20_0 <= '!') || (LA20_0 >= '#' && LA20_0 <= '[') ||
			         (LA20_0 >= ']' && LA20_0 <= '\uFFFE')))
				{
				alt20 = 2;
				}


			switch(alt20)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:246:14: EscapeSequence
				{
				mEscapeSequence();

				}
				break;
				case 2:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:246:31: ~ ( '\\\\' | '\"' )
				{
				if((input.LA(1) >= '\u0000' && input.LA(1) <= '!') || (input.LA(1) >= '#' && input.LA(1) <= '[') ||
				   (input.LA(1) >= ']' && input.LA(1) <= '\uFFFE'))
					{
					input.consume();

					}
				else
					{
					MismatchedSetException mse =
							new MismatchedSetException(null, input);
					recover(mse);
					throw mse;
					}


				}
				break;

				default:
					break loop20;
				}
			} while(true);

		match('\"');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end StringLiteral

// $ANTLR start EscapeSequence

public final void mEscapeSequence() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:251:9: ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' ) | UnicodeEscape | OctalEscape )
		int alt21 = 3;
		int LA21_0 = input.LA(1);

		if((LA21_0 == '\\'))
			{
			switch(input.LA(2))
				{
				case'u':
				{
				alt21 = 2;
				}
				break;
				case'\"':
				case'\'':
				case'\\':
				case'b':
				case'f':
				case'n':
				case'r':
				case't':
				{
				alt21 = 1;
				}
				break;
				case'0':
				case'1':
				case'2':
				case'3':
				case'4':
				case'5':
				case'6':
				case'7':
				{
				alt21 = 3;
				}
				break;
				default:
					NoViableAltException nvae =
							new NoViableAltException(
									"249:1: fragment EscapeSequence : ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' ) | UnicodeEscape | OctalEscape );",
									21, 1, input);

					throw nvae;
				}

			}
		else
			{
			NoViableAltException nvae =
					new NoViableAltException(
							"249:1: fragment EscapeSequence : ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' ) | UnicodeEscape | OctalEscape );",
							21, 0, input);

			throw nvae;
			}
		switch(alt21)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:251:9: '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' )
			{
			match('\\');
			if(input.LA(1) == '\"' || input.LA(1) == '\'' || input.LA(1) == '\\' || input.LA(1) == 'b' ||
			   input.LA(1) == 'f' || input.LA(1) == 'n' || input.LA(1) == 'r' || input.LA(1) == 't')
				{
				input.consume();

				}
			else
				{
				MismatchedSetException mse =
						new MismatchedSetException(null, input);
				recover(mse);
				throw mse;
				}


			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:252:9: UnicodeEscape
			{
			mUnicodeEscape();

			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:253:9: OctalEscape
			{
			mOctalEscape();

			}
			break;

			}
		}
	finally
		{
		}
}
// $ANTLR end EscapeSequence

// $ANTLR start OctalEscape

public final void mOctalEscape() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:258:9: ( '\\\\' ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) )
		int alt22 = 3;
		int LA22_0 = input.LA(1);

		if((LA22_0 == '\\'))
			{
			int LA22_1 = input.LA(2);

			if(((LA22_1 >= '0' && LA22_1 <= '3')))
				{
				int LA22_2 = input.LA(3);

				if(((LA22_2 >= '0' && LA22_2 <= '7')))
					{
					int LA22_4 = input.LA(4);

					if(((LA22_4 >= '0' && LA22_4 <= '7')))
						{
						alt22 = 1;
						}
					else
						{
						alt22 = 2;
						}
					}
				else
					{
					alt22 = 3;
					}
				}
			else if(((LA22_1 >= '4' && LA22_1 <= '7')))
				{
				int LA22_3 = input.LA(3);

				if(((LA22_3 >= '0' && LA22_3 <= '7')))
					{
					alt22 = 2;
					}
				else
					{
					alt22 = 3;
					}
				}
			else
				{
				NoViableAltException nvae =
						new NoViableAltException(
								"256:1: fragment OctalEscape : ( '\\\\' ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) );",
								22, 1, input);

				throw nvae;
				}
			}
		else
			{
			NoViableAltException nvae =
					new NoViableAltException(
							"256:1: fragment OctalEscape : ( '\\\\' ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) ( '0' .. '7' ) | '\\\\' ( '0' .. '7' ) );",
							22, 0, input);

			throw nvae;
			}
		switch(alt22)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:258:9: '\\\\' ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' )
			{
			match('\\');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:14: ( '0' .. '3' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:15: '0' .. '3'
			{
			matchRange('0', '3');

			}

			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:25: ( '0' .. '7' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:26: '0' .. '7'
			{
			matchRange('0', '7');

			}

			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:36: ( '0' .. '7' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:258:37: '0' .. '7'
			{
			matchRange('0', '7');

			}


			}
			break;
			case 2:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:259:9: '\\\\' ( '0' .. '7' ) ( '0' .. '7' )
			{
			match('\\');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:259:14: ( '0' .. '7' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:259:15: '0' .. '7'
			{
			matchRange('0', '7');

			}

			// /Users/rich/dev/clojure/src/jvm/Reader.g:259:25: ( '0' .. '7' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:259:26: '0' .. '7'
			{
			matchRange('0', '7');

			}


			}
			break;
			case 3:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:260:9: '\\\\' ( '0' .. '7' )
			{
			match('\\');
			// /Users/rich/dev/clojure/src/jvm/Reader.g:260:14: ( '0' .. '7' )
			// /Users/rich/dev/clojure/src/jvm/Reader.g:260:15: '0' .. '7'
			{
			matchRange('0', '7');

			}


			}
			break;

			}
		}
	finally
		{
		}
}
// $ANTLR end OctalEscape

// $ANTLR start UnicodeEscape

public final void mUnicodeEscape() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:265:9: ( '\\\\' 'u' HexDigit HexDigit HexDigit HexDigit )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:265:9: '\\\\' 'u' HexDigit HexDigit HexDigit HexDigit
		{
		match('\\');
		match('u');
		mHexDigit();
		mHexDigit();
		mHexDigit();
		mHexDigit();

		}

		}
	finally
		{
		}
}
// $ANTLR end UnicodeEscape

// $ANTLR start JavaIdentifier

public final void mJavaIdentifier() throws RecognitionException{
	try
		{
		int _type = JavaIdentifier;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:269:9: ( Letter ( Letter | JavaIDDigit )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:269:9: Letter ( Letter | JavaIDDigit )*
		{
		mLetter();
		// /Users/rich/dev/clojure/src/jvm/Reader.g:269:16: ( Letter | JavaIDDigit )*
		loop23:
		do
			{
			int alt23 = 2;
			int LA23_0 = input.LA(1);

			if((LA23_0 == '$' || (LA23_0 >= '0' && LA23_0 <= '9') || (LA23_0 >= 'A' && LA23_0 <= 'Z') ||
			    LA23_0 == '_' || (LA23_0 >= 'a' && LA23_0 <= 'z') || (LA23_0 >= '\u00C0' && LA23_0 <= '\u00D6') ||
			    (LA23_0 >= '\u00D8' && LA23_0 <= '\u00F6') || (LA23_0 >= '\u00F8' && LA23_0 <= '\u1FFF') ||
			    (LA23_0 >= '\u3040' && LA23_0 <= '\u318F') || (LA23_0 >= '\u3300' && LA23_0 <= '\u337F') ||
			    (LA23_0 >= '\u3400' && LA23_0 <= '\u3D2D') || (LA23_0 >= '\u4E00' && LA23_0 <= '\u9FFF') ||
			    (LA23_0 >= '\uF900' && LA23_0 <= '\uFAFF')))
				{
				alt23 = 1;
				}


			switch(alt23)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:
				{
				if(input.LA(1) == '$' || (input.LA(1) >= '0' && input.LA(1) <= '9') ||
				   (input.LA(1) >= 'A' && input.LA(1) <= 'Z') || input.LA(1) == '_' ||
				   (input.LA(1) >= 'a' && input.LA(1) <= 'z') || (input.LA(1) >= '\u00C0' && input.LA(1) <= '\u00D6') ||
				   (input.LA(1) >= '\u00D8' && input.LA(1) <= '\u00F6') ||
				   (input.LA(1) >= '\u00F8' && input.LA(1) <= '\u1FFF') ||
				   (input.LA(1) >= '\u3040' && input.LA(1) <= '\u318F') ||
				   (input.LA(1) >= '\u3300' && input.LA(1) <= '\u337F') ||
				   (input.LA(1) >= '\u3400' && input.LA(1) <= '\u3D2D') ||
				   (input.LA(1) >= '\u4E00' && input.LA(1) <= '\u9FFF') ||
				   (input.LA(1) >= '\uF900' && input.LA(1) <= '\uFAFF'))
					{
					input.consume();

					}
				else
					{
					MismatchedSetException mse =
							new MismatchedSetException(null, input);
					recover(mse);
					throw mse;
					}


				}
				break;

				default:
					break loop23;
				}
			} while(true);


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end JavaIdentifier

// $ANTLR start Identifier

public final void mIdentifier() throws RecognitionException{
	try
		{
		int _type = Identifier;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:273:9: ( Letter ( '-' | Letter | JavaIDDigit )* )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:273:9: Letter ( '-' | Letter | JavaIDDigit )*
		{
		mLetter();
		// /Users/rich/dev/clojure/src/jvm/Reader.g:273:16: ( '-' | Letter | JavaIDDigit )*
		loop24:
		do
			{
			int alt24 = 2;
			int LA24_0 = input.LA(1);

			if((LA24_0 == '$' || LA24_0 == '-' || (LA24_0 >= '0' && LA24_0 <= '9') ||
			    (LA24_0 >= 'A' && LA24_0 <= 'Z') || LA24_0 == '_' || (LA24_0 >= 'a' && LA24_0 <= 'z') ||
			    (LA24_0 >= '\u00C0' && LA24_0 <= '\u00D6') || (LA24_0 >= '\u00D8' && LA24_0 <= '\u00F6') ||
			    (LA24_0 >= '\u00F8' && LA24_0 <= '\u1FFF') || (LA24_0 >= '\u3040' && LA24_0 <= '\u318F') ||
			    (LA24_0 >= '\u3300' && LA24_0 <= '\u337F') || (LA24_0 >= '\u3400' && LA24_0 <= '\u3D2D') ||
			    (LA24_0 >= '\u4E00' && LA24_0 <= '\u9FFF') || (LA24_0 >= '\uF900' && LA24_0 <= '\uFAFF')))
				{
				alt24 = 1;
				}


			switch(alt24)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:
				{
				if(input.LA(1) == '$' || input.LA(1) == '-' || (input.LA(1) >= '0' && input.LA(1) <= '9') ||
				   (input.LA(1) >= 'A' && input.LA(1) <= 'Z') || input.LA(1) == '_' ||
				   (input.LA(1) >= 'a' && input.LA(1) <= 'z') || (input.LA(1) >= '\u00C0' && input.LA(1) <= '\u00D6') ||
				   (input.LA(1) >= '\u00D8' && input.LA(1) <= '\u00F6') ||
				   (input.LA(1) >= '\u00F8' && input.LA(1) <= '\u1FFF') ||
				   (input.LA(1) >= '\u3040' && input.LA(1) <= '\u318F') ||
				   (input.LA(1) >= '\u3300' && input.LA(1) <= '\u337F') ||
				   (input.LA(1) >= '\u3400' && input.LA(1) <= '\u3D2D') ||
				   (input.LA(1) >= '\u4E00' && input.LA(1) <= '\u9FFF') ||
				   (input.LA(1) >= '\uF900' && input.LA(1) <= '\uFAFF'))
					{
					input.consume();

					}
				else
					{
					MismatchedSetException mse =
							new MismatchedSetException(null, input);
					recover(mse);
					throw mse;
					}


				}
				break;

				default:
					break loop24;
				}
			} while(true);


		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end Identifier

// $ANTLR start MethodIdentifier

public final void mMethodIdentifier() throws RecognitionException{
	try
		{
		int _type = MethodIdentifier;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:277:9: ( Letter ( Letter | JavaIDDigit )* '(' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:277:9: Letter ( Letter | JavaIDDigit )* '('
		{
		mLetter();
		// /Users/rich/dev/clojure/src/jvm/Reader.g:277:16: ( Letter | JavaIDDigit )*
		loop25:
		do
			{
			int alt25 = 2;
			int LA25_0 = input.LA(1);

			if((LA25_0 == '$' || (LA25_0 >= '0' && LA25_0 <= '9') || (LA25_0 >= 'A' && LA25_0 <= 'Z') ||
			    LA25_0 == '_' || (LA25_0 >= 'a' && LA25_0 <= 'z') || (LA25_0 >= '\u00C0' && LA25_0 <= '\u00D6') ||
			    (LA25_0 >= '\u00D8' && LA25_0 <= '\u00F6') || (LA25_0 >= '\u00F8' && LA25_0 <= '\u1FFF') ||
			    (LA25_0 >= '\u3040' && LA25_0 <= '\u318F') || (LA25_0 >= '\u3300' && LA25_0 <= '\u337F') ||
			    (LA25_0 >= '\u3400' && LA25_0 <= '\u3D2D') || (LA25_0 >= '\u4E00' && LA25_0 <= '\u9FFF') ||
			    (LA25_0 >= '\uF900' && LA25_0 <= '\uFAFF')))
				{
				alt25 = 1;
				}


			switch(alt25)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:
				{
				if(input.LA(1) == '$' || (input.LA(1) >= '0' && input.LA(1) <= '9') ||
				   (input.LA(1) >= 'A' && input.LA(1) <= 'Z') || input.LA(1) == '_' ||
				   (input.LA(1) >= 'a' && input.LA(1) <= 'z') || (input.LA(1) >= '\u00C0' && input.LA(1) <= '\u00D6') ||
				   (input.LA(1) >= '\u00D8' && input.LA(1) <= '\u00F6') ||
				   (input.LA(1) >= '\u00F8' && input.LA(1) <= '\u1FFF') ||
				   (input.LA(1) >= '\u3040' && input.LA(1) <= '\u318F') ||
				   (input.LA(1) >= '\u3300' && input.LA(1) <= '\u337F') ||
				   (input.LA(1) >= '\u3400' && input.LA(1) <= '\u3D2D') ||
				   (input.LA(1) >= '\u4E00' && input.LA(1) <= '\u9FFF') ||
				   (input.LA(1) >= '\uF900' && input.LA(1) <= '\uFAFF'))
					{
					input.consume();

					}
				else
					{
					MismatchedSetException mse =
							new MismatchedSetException(null, input);
					recover(mse);
					throw mse;
					}


				}
				break;

				default:
					break loop25;
				}
			} while(true);

		match('(');

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end MethodIdentifier

// $ANTLR start Letter

public final void mLetter() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:284:8: ( '\\u0024' | '\\u0041' .. '\\u005a' | '\\u005f' | '\\u0061' .. '\\u007a' | '\\u00c0' .. '\\u00d6' | '\\u00d8' .. '\\u00f6' | '\\u00f8' .. '\\u00ff' | '\\u0100' .. '\\u1fff' | '\\u3040' .. '\\u318f' | '\\u3300' .. '\\u337f' | '\\u3400' .. '\\u3d2d' | '\\u4e00' .. '\\u9fff' | '\\uf900' .. '\\ufaff' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:
		{
		if(input.LA(1) == '$' || (input.LA(1) >= 'A' && input.LA(1) <= 'Z') || input.LA(1) == '_' ||
		   (input.LA(1) >= 'a' && input.LA(1) <= 'z') || (input.LA(1) >= '\u00C0' && input.LA(1) <= '\u00D6') ||
		   (input.LA(1) >= '\u00D8' && input.LA(1) <= '\u00F6') ||
		   (input.LA(1) >= '\u00F8' && input.LA(1) <= '\u1FFF') ||
		   (input.LA(1) >= '\u3040' && input.LA(1) <= '\u318F') ||
		   (input.LA(1) >= '\u3300' && input.LA(1) <= '\u337F') ||
		   (input.LA(1) >= '\u3400' && input.LA(1) <= '\u3D2D') ||
		   (input.LA(1) >= '\u4E00' && input.LA(1) <= '\u9FFF') || (input.LA(1) >= '\uF900' && input.LA(1) <= '\uFAFF'))
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}


		}

		}
	finally
		{
		}
}
// $ANTLR end Letter

// $ANTLR start JavaIDDigit

public final void mJavaIDDigit() throws RecognitionException{
	try
		{
		// /Users/rich/dev/clojure/src/jvm/Reader.g:302:6: ( '\\u0030' .. '\\u0039' | '\\u0660' .. '\\u0669' | '\\u06f0' .. '\\u06f9' | '\\u0966' .. '\\u096f' | '\\u09e6' .. '\\u09ef' | '\\u0a66' .. '\\u0a6f' | '\\u0ae6' .. '\\u0aef' | '\\u0b66' .. '\\u0b6f' | '\\u0be7' .. '\\u0bef' | '\\u0c66' .. '\\u0c6f' | '\\u0ce6' .. '\\u0cef' | '\\u0d66' .. '\\u0d6f' | '\\u0e50' .. '\\u0e59' | '\\u0ed0' .. '\\u0ed9' | '\\u1040' .. '\\u1049' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:
		{
		if((input.LA(1) >= '0' && input.LA(1) <= '9') || (input.LA(1) >= '\u0660' && input.LA(1) <= '\u0669') ||
		   (input.LA(1) >= '\u06F0' && input.LA(1) <= '\u06F9') ||
		   (input.LA(1) >= '\u0966' && input.LA(1) <= '\u096F') ||
		   (input.LA(1) >= '\u09E6' && input.LA(1) <= '\u09EF') ||
		   (input.LA(1) >= '\u0A66' && input.LA(1) <= '\u0A6F') ||
		   (input.LA(1) >= '\u0AE6' && input.LA(1) <= '\u0AEF') ||
		   (input.LA(1) >= '\u0B66' && input.LA(1) <= '\u0B6F') ||
		   (input.LA(1) >= '\u0BE7' && input.LA(1) <= '\u0BEF') ||
		   (input.LA(1) >= '\u0C66' && input.LA(1) <= '\u0C6F') ||
		   (input.LA(1) >= '\u0CE6' && input.LA(1) <= '\u0CEF') ||
		   (input.LA(1) >= '\u0D66' && input.LA(1) <= '\u0D6F') ||
		   (input.LA(1) >= '\u0E50' && input.LA(1) <= '\u0E59') ||
		   (input.LA(1) >= '\u0ED0' && input.LA(1) <= '\u0ED9') || (input.LA(1) >= '\u1040' && input.LA(1) <= '\u1049'))
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}


		}

		}
	finally
		{
		}
}
// $ANTLR end JavaIDDigit

// $ANTLR start WS

public final void mWS() throws RecognitionException{
	try
		{
		int _type = WS;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:319:8: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' ) )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:319:8: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )
		{
		if((input.LA(1) >= '\t' && input.LA(1) <= '\n') || (input.LA(1) >= '\f' && input.LA(1) <= '\r') ||
		   input.LA(1) == ' ')
			{
			input.consume();

			}
		else
			{
			MismatchedSetException mse =
					new MismatchedSetException(null, input);
			recover(mse);
			throw mse;
			}

		channel = HIDDEN;

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end WS

// $ANTLR start COMMENT

public final void mCOMMENT() throws RecognitionException{
	try
		{
		int _type = COMMENT;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:323:9: ( '#|' ( options {greedy=false; } : . )* '|#' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:323:9: '#|' ( options {greedy=false; } : . )* '|#'
		{
		match("#|");

		// /Users/rich/dev/clojure/src/jvm/Reader.g:323:14: ( options {greedy=false; } : . )*
		loop26:
		do
			{
			int alt26 = 2;
			int LA26_0 = input.LA(1);

			if((LA26_0 == '|'))
				{
				int LA26_1 = input.LA(2);

				if((LA26_1 == '#'))
					{
					alt26 = 2;
					}
				else if(((LA26_1 >= '\u0000' && LA26_1 <= '\"') || (LA26_1 >= '$' && LA26_1 <= '\uFFFE')))
					{
					alt26 = 1;
					}


				}
			else if(((LA26_0 >= '\u0000' && LA26_0 <= '{') || (LA26_0 >= '}' && LA26_0 <= '\uFFFE')))
				{
				alt26 = 1;
				}


			switch(alt26)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:323:42: .
				{
				matchAny();

				}
				break;

				default:
					break loop26;
				}
			} while(true);

		match("|#");

		channel = HIDDEN;

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end COMMENT

// $ANTLR start LINE_COMMENT

public final void mLINE_COMMENT() throws RecognitionException{
	try
		{
		int _type = LINE_COMMENT;
		// /Users/rich/dev/clojure/src/jvm/Reader.g:327:7: ( ';' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? '\\n' )
		// /Users/rich/dev/clojure/src/jvm/Reader.g:327:7: ';' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? '\\n'
		{
		match(';');
		// /Users/rich/dev/clojure/src/jvm/Reader.g:327:11: (~ ( '\\n' | '\\r' ) )*
		loop27:
		do
			{
			int alt27 = 2;
			int LA27_0 = input.LA(1);

			if(((LA27_0 >= '\u0000' && LA27_0 <= '\t') || (LA27_0 >= '\u000B' && LA27_0 <= '\f') ||
			    (LA27_0 >= '\u000E' && LA27_0 <= '\uFFFE')))
				{
				alt27 = 1;
				}


			switch(alt27)
				{
				case 1:
					// /Users/rich/dev/clojure/src/jvm/Reader.g:327:11: ~ ( '\\n' | '\\r' )
				{
				if((input.LA(1) >= '\u0000' && input.LA(1) <= '\t') ||
				   (input.LA(1) >= '\u000B' && input.LA(1) <= '\f') ||
				   (input.LA(1) >= '\u000E' && input.LA(1) <= '\uFFFE'))
					{
					input.consume();

					}
				else
					{
					MismatchedSetException mse =
							new MismatchedSetException(null, input);
					recover(mse);
					throw mse;
					}


				}
				break;

				default:
					break loop27;
				}
			} while(true);

		// /Users/rich/dev/clojure/src/jvm/Reader.g:327:25: ( '\\r' )?
		int alt28 = 2;
		int LA28_0 = input.LA(1);

		if((LA28_0 == '\r'))
			{
			alt28 = 1;
			}
		switch(alt28)
			{
			case 1:
				// /Users/rich/dev/clojure/src/jvm/Reader.g:327:25: '\\r'
			{
			match('\r');

			}
			break;

			}

		match('\n');
		channel = HIDDEN;

		}

		this.type = _type;
		}
	finally
		{
		}
}
// $ANTLR end LINE_COMMENT

public void mTokens() throws RecognitionException{
	// /Users/rich/dev/clojure/src/jvm/Reader.g:1:10: ( T28 | T29 | T30 | T31 | T32 | T33 | T34 | T35 | T36 | T37 | Comma | TrueToken | NullToken | DotDot | HexLiteral | DecimalLiteral | OctalLiteral | FloatingPointLiteral | CharacterLiteral | StringLiteral | JavaIdentifier | Identifier | MethodIdentifier | WS | COMMENT | LINE_COMMENT )
	int alt29 = 26;
	alt29 = dfa29.predict(input);
	switch(alt29)
		{
		case 1:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:10: T28
		{
		mT28();

		}
		break;
		case 2:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:14: T29
		{
		mT29();

		}
		break;
		case 3:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:18: T30
		{
		mT30();

		}
		break;
		case 4:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:22: T31
		{
		mT31();

		}
		break;
		case 5:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:26: T32
		{
		mT32();

		}
		break;
		case 6:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:30: T33
		{
		mT33();

		}
		break;
		case 7:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:34: T34
		{
		mT34();

		}
		break;
		case 8:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:38: T35
		{
		mT35();

		}
		break;
		case 9:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:42: T36
		{
		mT36();

		}
		break;
		case 10:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:46: T37
		{
		mT37();

		}
		break;
		case 11:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:50: Comma
		{
		mComma();

		}
		break;
		case 12:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:56: TrueToken
		{
		mTrueToken();

		}
		break;
		case 13:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:66: NullToken
		{
		mNullToken();

		}
		break;
		case 14:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:76: DotDot
		{
		mDotDot();

		}
		break;
		case 15:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:83: HexLiteral
		{
		mHexLiteral();

		}
		break;
		case 16:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:94: DecimalLiteral
		{
		mDecimalLiteral();

		}
		break;
		case 17:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:109: OctalLiteral
		{
		mOctalLiteral();

		}
		break;
		case 18:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:122: FloatingPointLiteral
		{
		mFloatingPointLiteral();

		}
		break;
		case 19:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:143: CharacterLiteral
		{
		mCharacterLiteral();

		}
		break;
		case 20:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:160: StringLiteral
		{
		mStringLiteral();

		}
		break;
		case 21:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:174: JavaIdentifier
		{
		mJavaIdentifier();

		}
		break;
		case 22:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:189: Identifier
		{
		mIdentifier();

		}
		break;
		case 23:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:200: MethodIdentifier
		{
		mMethodIdentifier();

		}
		break;
		case 24:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:217: WS
		{
		mWS();

		}
		break;
		case 25:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:220: COMMENT
		{
		mCOMMENT();

		}
		break;
		case 26:
			// /Users/rich/dev/clojure/src/jvm/Reader.g:1:228: LINE_COMMENT
		{
		mLINE_COMMENT();

		}
		break;

		}

}


protected DFA16 dfa16 = new DFA16(this);
protected DFA29 dfa29 = new DFA29(this);
static final String DFA16_eotS =
		"\7\uffff\1\10\2\uffff";
static final String DFA16_eofS =
		"\12\uffff";
static final String DFA16_minS =
		"\2\56\1\uffff\1\53\2\uffff\2\60\2\uffff";
static final String DFA16_maxS =
		"\1\71\1\146\1\uffff\1\71\2\uffff\1\71\1\146\2\uffff";
static final String DFA16_acceptS =
		"\2\uffff\1\2\1\uffff\1\4\1\1\2\uffff\2\3";
static final String DFA16_specialS =
		"\12\uffff}>";
static final String[] DFA16_transitionS = {
		"\1\2\1\uffff\12\1",
		"\1\5\1\uffff\12\1\12\uffff\1\4\1\3\1\4\35\uffff\1\4\1\3\1\4",
		"",
		"\1\6\1\uffff\1\6\2\uffff\12\7",
		"",
		"",
		"\12\7",
		"\12\7\12\uffff\1\11\1\uffff\1\11\35\uffff\1\11\1\uffff\1\11",
		"",
		""
};

static final short[] DFA16_eot = DFA.unpackEncodedString(DFA16_eotS);
static final short[] DFA16_eof = DFA.unpackEncodedString(DFA16_eofS);
static final char[] DFA16_min = DFA.unpackEncodedStringToUnsignedChars(DFA16_minS);
static final char[] DFA16_max = DFA.unpackEncodedStringToUnsignedChars(DFA16_maxS);
static final short[] DFA16_accept = DFA.unpackEncodedString(DFA16_acceptS);
static final short[] DFA16_special = DFA.unpackEncodedString(DFA16_specialS);
static final short[][] DFA16_transition;

static
	{
	int numStates = DFA16_transitionS.length;
	DFA16_transition = new short[numStates][];
	for(int i = 0; i < numStates; i++)
		{
		DFA16_transition[i] = DFA.unpackEncodedString(DFA16_transitionS[i]);
		}
	}

class DFA16 extends DFA{

	public DFA16(BaseRecognizer recognizer){
		this.recognizer = recognizer;
		this.decisionNumber = 16;
		this.eot = DFA16_eot;
		this.eof = DFA16_eof;
		this.min = DFA16_min;
		this.max = DFA16_max;
		this.accept = DFA16_accept;
		this.special = DFA16_special;
		this.transition = DFA16_transition;
	}

	public String getDescription(){
		return "228:1: FloatingPointLiteral : ( ( '0' .. '9' )+ '.' ( '0' .. '9' )* ( Exponent )? ( FloatTypeSuffix )? | '.' ( '0' .. '9' )+ ( Exponent )? ( FloatTypeSuffix )? | ( '0' .. '9' )+ Exponent ( FloatTypeSuffix )? | ( '0' .. '9' )+ ( Exponent )? FloatTypeSuffix );";
	}
}

static final String DFA29_eotS =
		"\12\uffff\1\31\1\uffff\2\33\2\42\2\uffff\1\33\7\uffff\1\33\1\uffff" +
		"\1\33\2\uffff\1\33\1\uffff\1\46\1\uffff\1\42\2\33\1\uffff\1\51\1" +
		"\52\2\uffff";
static final String DFA29_eofS =
		"\53\uffff";
static final String DFA29_minS =
		"\1\11\10\uffff\1\136\1\56\1\uffff\2\44\2\56\2\uffff\1\44\7\uffff" +
		"\1\44\1\uffff\1\44\2\uffff\1\44\1\uffff\1\56\1\uffff\1\56\2\44\1" +
		"\uffff\2\44\2\uffff";
static final String DFA29_maxS =
		"\1\ufaff\10\uffff\1\174\1\71\1\uffff\2\ufaff\1\170\1\146\2\uffff" +
		"\1\ufaff\7\uffff\1\ufaff\1\uffff\1\ufaff\2\uffff\1\ufaff\1\uffff" +
		"\1\146\1\uffff\1\146\2\ufaff\1\uffff\2\ufaff\2\uffff";
static final String DFA29_acceptS =
		"\1\uffff\1\1\1\2\1\3\1\4\1\5\1\6\1\7\1\10\2\uffff\1\13\4\uffff\1" +
		"\23\1\24\1\uffff\1\30\1\32\1\31\1\11\1\16\1\22\1\12\1\uffff\1\25" +
		"\1\uffff\1\27\1\26\1\uffff\1\17\1\uffff\1\20\3\uffff\1\21\2\uffff" +
		"\1\14\1\15";
static final String DFA29_specialS =
		"\53\uffff}>";
static final String[] DFA29_transitionS = {
		"\2\23\1\uffff\2\23\22\uffff\1\23\1\uffff\1\21\1\11\1\22\3\uffff" +
		"\1\1\1\2\2\uffff\1\13\1\uffff\1\12\1\7\1\16\11\17\1\10\1\24" +
		"\5\uffff\32\22\1\3\1\20\1\4\1\uffff\1\22\1\uffff\15\22\1\15" +
		"\5\22\1\14\6\22\1\5\1\uffff\1\6\102\uffff\27\22\1\uffff\37\22" +
		"\1\uffff\u1f08\22\u1040\uffff\u0150\22\u0170\uffff\u0080\22" +
		"\u0080\uffff\u092e\22\u10d2\uffff\u5200\22\u5900\uffff\u0200" +
		"\22",
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		"\1\26\35\uffff\1\25",
		"\1\27\1\uffff\12\30",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\21\34\1\32\10\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\24\34\1\37\5\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"\1\30\1\uffff\10\41\2\30\12\uffff\3\30\21\uffff\1\40\13\uffff" +
		"\3\30\21\uffff\1\40",
		"\1\30\1\uffff\12\43\12\uffff\3\30\35\uffff\3\30",
		"",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\32\34\105\uffff\27\34\1\uffff\37\34\1" +
		"\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080\34\u0080" +
		"\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200\34",
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\24\34\1\44\5\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\32\34\105\uffff\27\34\1\uffff\37\34\1" +
		"\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080\34\u0080" +
		"\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200\34",
		"",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\13\34\1\45\16\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"",
		"\1\30\1\uffff\10\41\2\30\12\uffff\3\30\35\uffff\3\30",
		"",
		"\1\30\1\uffff\12\43\12\uffff\3\30\35\uffff\3\30",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\4\34\1\47\25\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\13\34\1\50\16\34\105\uffff\27\34\1\uffff" +
		"\37\34\1\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080" +
		"\34\u0080\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200" +
		"\34",
		"",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\32\34\105\uffff\27\34\1\uffff\37\34\1" +
		"\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080\34\u0080" +
		"\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200\34",
		"\1\34\3\uffff\1\35\4\uffff\1\36\2\uffff\12\34\7\uffff\32\34" +
		"\4\uffff\1\34\1\uffff\32\34\105\uffff\27\34\1\uffff\37\34\1" +
		"\uffff\u1f08\34\u1040\uffff\u0150\34\u0170\uffff\u0080\34\u0080" +
		"\uffff\u092e\34\u10d2\uffff\u5200\34\u5900\uffff\u0200\34",
		"",
		""
};

static final short[] DFA29_eot = DFA.unpackEncodedString(DFA29_eotS);
static final short[] DFA29_eof = DFA.unpackEncodedString(DFA29_eofS);
static final char[] DFA29_min = DFA.unpackEncodedStringToUnsignedChars(DFA29_minS);
static final char[] DFA29_max = DFA.unpackEncodedStringToUnsignedChars(DFA29_maxS);
static final short[] DFA29_accept = DFA.unpackEncodedString(DFA29_acceptS);
static final short[] DFA29_special = DFA.unpackEncodedString(DFA29_specialS);
static final short[][] DFA29_transition;

static
	{
	int numStates = DFA29_transitionS.length;
	DFA29_transition = new short[numStates][];
	for(int i = 0; i < numStates; i++)
		{
		DFA29_transition[i] = DFA.unpackEncodedString(DFA29_transitionS[i]);
		}
	}

class DFA29 extends DFA{

	public DFA29(BaseRecognizer recognizer){
		this.recognizer = recognizer;
		this.decisionNumber = 29;
		this.eot = DFA29_eot;
		this.eof = DFA29_eof;
		this.min = DFA29_min;
		this.max = DFA29_max;
		this.accept = DFA29_accept;
		this.special = DFA29_special;
		this.transition = DFA29_transition;
	}

	public String getDescription(){
		return "1:1: Tokens : ( T28 | T29 | T30 | T31 | T32 | T33 | T34 | T35 | T36 | T37 | Comma | TrueToken | NullToken | DotDot | HexLiteral | DecimalLiteral | OctalLiteral | FloatingPointLiteral | CharacterLiteral | StringLiteral | JavaIdentifier | Identifier | MethodIdentifier | WS | COMMENT | LINE_COMMENT );";
	}
}


}