/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class EdnReader{

static IFn[] macros = new IFn[256];
static IFn[] dispatchMacros = new IFn[256];
static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^/]].*/)?([\\D&&[^/]][^/]*)");
static Pattern intPat =
		Pattern.compile(
				"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
static Pattern ratioPat = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
static Pattern floatPat = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");
static final Symbol SLASH = Symbol.intern("/");

static
	{
	macros['"'] = new StringReader();
	macros[';'] = new CommentReader();
	macros['^'] = new MetaReader();
	macros['('] = new ListReader();
	macros[')'] = new UnmatchedDelimiterReader();
	macros['['] = new VectorReader();
	macros[']'] = new UnmatchedDelimiterReader();
	macros['{'] = new MapReader();
	macros['}'] = new UnmatchedDelimiterReader();
	macros['\\'] = new CharacterReader();
	macros['#'] = new DispatchReader();


	dispatchMacros['^'] = new MetaReader();
	//dispatchMacros['"'] = new RegexReader();
	dispatchMacros['{'] = new SetReader();
	dispatchMacros['<'] = new UnreadableReader();
	dispatchMacros['_'] = new DiscardReader();
	}

static public Object readString(String s){
	PushbackReader r = new PushbackReader(new java.io.StringReader(s));
	try {
	return EdnReader.read(r, true, null, false);
	}
	catch(Exception e) {
	throw Util.sneakyThrow(e);
	}
}

static boolean isWhitespace(int ch){
	return Character.isWhitespace(ch) || ch == ',';
}

static void unread(PushbackReader r, int ch) {
	if(ch != -1)
		try
			{
			r.unread(ch);
			}
		catch(IOException e)
			{
			throw Util.sneakyThrow(e);
			}
}

public static class ReaderException extends RuntimeException{
	final int line;
	final int column;

	public ReaderException(int line, int column, Throwable cause){
		super(cause);
		this.line = line;
		this.column = column;
	}
}

static public int read1(Reader r){
	try
		{
		return r.read();
		}
	catch(IOException e)
		{
		throw Util.sneakyThrow(e);
		}
}

static public Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive)
{

	try
		{
		for(; ;)
			{
			int ch = read1(r);

			while(isWhitespace(ch))
				ch = read1(r);

			if(ch == -1)
				{
				if(eofIsError)
					throw Util.runtimeException("EOF while reading");
				return eofValue;
				}

			if(Character.isDigit(ch))
				{
				Object n = readNumber(r, (char) ch);
				if(RT.suppressRead())
					return null;
				return n;
				}

			IFn macroFn = getMacro(ch);
			if(macroFn != null)
				{
				Object ret = macroFn.invoke(r, (char) ch);
				if(RT.suppressRead())
					return null;
				//no op macros return the reader
				if(ret == r)
					continue;
				return ret;
				}

			if(ch == '+' || ch == '-')
				{
				int ch2 = read1(r);
				if(Character.isDigit(ch2))
					{
					unread(r, ch2);
					Object n = readNumber(r, (char) ch);
					if(RT.suppressRead())
						return null;
					return n;
					}
				unread(r, ch2);
				}

			String token = readToken(r, (char) ch);
			if(RT.suppressRead())
				return null;
			return interpretToken(token);
			}
		}
	catch(Exception e)
		{
		if(isRecursive || !(r instanceof LineNumberingPushbackReader))
			throw Util.sneakyThrow(e);
		LineNumberingPushbackReader rdr = (LineNumberingPushbackReader) r;
		//throw Util.runtimeException(String.format("ReaderError:(%d,1) %s", rdr.getLineNumber(), e.getMessage()), e);
		throw new ReaderException(rdr.getLineNumber(), rdr.getColumnNumber(), e);
		}
}

static private String readToken(PushbackReader r, char initch) {
	StringBuilder sb = new StringBuilder();
	sb.append(initch);

	for(; ;)
		{
		int ch = read1(r);
		if(ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch))
			{
			unread(r, ch);
			return sb.toString();
			}
		sb.append((char) ch);
		}
}

static private Object readNumber(PushbackReader r, char initch) {
	StringBuilder sb = new StringBuilder();
	sb.append(initch);

	for(; ;)
		{
		int ch = read1(r);
		if(ch == -1 || isWhitespace(ch) || isMacro(ch))
			{
			unread(r, ch);
			break;
			}
		sb.append((char) ch);
		}

	String s = sb.toString();
	Object n = matchNumber(s);
	if(n == null)
		throw new NumberFormatException("Invalid number: " + s);
	return n;
}

static private int readUnicodeChar(String token, int offset, int length, int base) {
	if(token.length() != offset + length)
		throw new IllegalArgumentException("Invalid unicode character: \\" + token);
	int uc = 0;
	for(int i = offset; i < offset + length; ++i)
		{
		int d = Character.digit(token.charAt(i), base);
		if(d == -1)
			throw new IllegalArgumentException("Invalid digit: " + token.charAt(i));
		uc = uc * base + d;
		}
	return (char) uc;
}

static private int readUnicodeChar(PushbackReader r, int initch, int base, int length, boolean exact) {
	int uc = Character.digit(initch, base);
	if(uc == -1)
		throw new IllegalArgumentException("Invalid digit: " + (char) initch);
	int i = 1;
	for(; i < length; ++i)
		{
		int ch = read1(r);
		if(ch == -1 || isWhitespace(ch) || isMacro(ch))
			{
			unread(r, ch);
			break;
			}
		int d = Character.digit(ch, base);
		if(d == -1)
			throw new IllegalArgumentException("Invalid digit: " + (char) ch);
		uc = uc * base + d;
		}
	if(i != length && exact)
		throw new IllegalArgumentException("Invalid character length: " + i + ", should be: " + length);
	return uc;
}

static private Object interpretToken(String s) {
	if(s.equals("nil"))
		{
		return null;
		}
	else if(s.equals("true"))
		{
		return RT.T;
		}
	else if(s.equals("false"))
		{
		return RT.F;
		}
	else if(s.equals("/"))
		{
		return SLASH;
		}

	Object ret = null;

	ret = matchSymbol(s);
	if(ret != null)
		return ret;

	throw Util.runtimeException("Invalid token: " + s);
}


private static Object matchSymbol(String s){
	Matcher m = symbolPat.matcher(s);
	if(m.matches())
		{
		int gc = m.groupCount();
		String ns = m.group(1);
		String name = m.group(2);
		if(ns != null && ns.endsWith(":/")
		   || name.endsWith(":")
		   || s.indexOf("::", 1) != -1)
			return null;
		if(s.startsWith("::"))
			{
			return null;
			}
		boolean isKeyword = s.charAt(0) == ':';
		Symbol sym = Symbol.intern(s.substring(isKeyword ? 1 : 0));
		if(isKeyword)
			return Keyword.intern(sym);
		return sym;
		}
	return null;
}


private static Object matchNumber(String s){
	Matcher m = intPat.matcher(s);
	if(m.matches())
		{
		if(m.group(2) != null)
			{
			if(m.group(8) != null)
				return BigInt.ZERO;
			return Numbers.num(0);
			}
		boolean negate = (m.group(1).equals("-"));
		String n;
		int radix = 10;
		if((n = m.group(3)) != null)
			radix = 10;
		else if((n = m.group(4)) != null)
			radix = 16;
		else if((n = m.group(5)) != null)
			radix = 8;
		else if((n = m.group(7)) != null)
			radix = Integer.parseInt(m.group(6));
		if(n == null)
			return null;
		BigInteger bn = new BigInteger(n, radix);
		if(negate)
			bn = bn.negate();
		if(m.group(8) != null)
			return BigInt.fromBigInteger(bn);
		return bn.bitLength() < 64 ?
		       Numbers.num(bn.longValue())
		                           : BigInt.fromBigInteger(bn);
		}
	m = floatPat.matcher(s);
	if(m.matches())
		{
		if(m.group(4) != null)
			return new BigDecimal(m.group(1));
		return Double.parseDouble(s);
		}
	m = ratioPat.matcher(s);
	if(m.matches())
		{
		String numerator = m.group(1);
		if (numerator.startsWith("+")) numerator = numerator.substring(1);

		return Numbers.divide(Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(numerator))),
		                      Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(m.group(2)))));
		}
	return null;
}

static private IFn getMacro(int ch){
	if(ch < macros.length)
		return macros[ch];
	return null;
}

static private boolean isMacro(int ch){
	return (ch < macros.length && macros[ch] != null);
}

static private boolean isTerminatingMacro(int ch){
	return (ch != '#' && ch != '\'' && isMacro(ch));
}

/*
public static class RegexReader extends AFn{

	static StringReader stringrdr = new StringReader();

	public Object invoke(Object reader, Object doublequote) {
		StringBuilder sb = new StringBuilder();
		Reader r = (Reader) reader;
		for(int ch = read1(r); ch != '"'; ch = read1(r))
			{
			if(ch == -1)
				throw Util.runtimeException("EOF while reading regex");
			sb.append( (char) ch );
			if(ch == '\\')	//escape
				{
				ch = read1(r);
				if(ch == -1)
					throw Util.runtimeException("EOF while reading regex");
				sb.append( (char) ch ) ;
				}
			}
		return Pattern.compile(sb.toString());
	}
}
*/

public static class StringReader extends AFn{
	public Object invoke(Object reader, Object doublequote) {
		StringBuilder sb = new StringBuilder();
		Reader r = (Reader) reader;

		for(int ch = read1(r); ch != '"'; ch = read1(r))
			{
			if(ch == -1)
				throw Util.runtimeException("EOF while reading string");
			if(ch == '\\')	//escape
				{
				ch = read1(r);
				if(ch == -1)
					throw Util.runtimeException("EOF while reading string");
				switch(ch)
					{
					case 't':
						ch = '\t';
						break;
					case 'r':
						ch = '\r';
						break;
					case 'n':
						ch = '\n';
						break;
					case '\\':
						break;
					case '"':
						break;
					case 'b':
						ch = '\b';
						break;
					case 'f':
						ch = '\f';
						break;
					case 'u':
					{
					ch = read1(r);
					if (Character.digit(ch, 16) == -1)
						throw Util.runtimeException("Invalid unicode escape: \\u" + (char) ch);
					ch = readUnicodeChar((PushbackReader) r, ch, 16, 4, true);
					break;
					}
					default:
					{
					if(Character.isDigit(ch))
						{
						ch = readUnicodeChar((PushbackReader) r, ch, 8, 3, false);
						if(ch > 0377)
							throw Util.runtimeException("Octal escape sequence must be in range [0, 377].");
						}
					else
						throw Util.runtimeException("Unsupported escape character: \\" + (char) ch);
					}
					}
				}
			sb.append((char) ch);
			}
		return sb.toString();
	}
}

public static class CommentReader extends AFn{
	public Object invoke(Object reader, Object semicolon) {
		Reader r = (Reader) reader;
		int ch;
		do
			{
			ch = read1(r);
			} while(ch != -1 && ch != '\n' && ch != '\r');
		return r;
	}

}

public static class DiscardReader extends AFn{
	public Object invoke(Object reader, Object underscore) {
		PushbackReader r = (PushbackReader) reader;
		read(r, true, null, true);
		return r;
	}
}

public static class DispatchReader extends AFn{
	public Object invoke(Object reader, Object hash) {
		int ch = read1((Reader) reader);
		if(ch == -1)
			throw Util.runtimeException("EOF while reading character");
		IFn fn = dispatchMacros[ch];

		if(fn == null) {
			throw Util.runtimeException(String.format("No dispatch macro for: %c", (char) ch));
		}
		return fn.invoke(reader, ch);
	}
}

public static class MetaReader extends AFn{
	public Object invoke(Object reader, Object caret) {
		PushbackReader r = (PushbackReader) reader;
		int line = -1;
		int column = -1;
		if(r instanceof LineNumberingPushbackReader)
			{
			line = ((LineNumberingPushbackReader) r).getLineNumber();
			column = ((LineNumberingPushbackReader) r).getColumnNumber()-1;
			}
		Object meta = read(r, true, null, true);
		if(meta instanceof Symbol || meta instanceof String)
			meta = RT.map(RT.TAG_KEY, meta);
		else if (meta instanceof Keyword)
			meta = RT.map(meta, RT.T);
		else if(!(meta instanceof IPersistentMap))
			throw new IllegalArgumentException("Metadata must be Symbol,Keyword,String or Map");

		Object o = read(r, true, null, true);
		if(o instanceof IMeta)
			{
			if(line != -1 && o instanceof ISeq)
				{
				meta = ((IPersistentMap) meta).assoc(RT.LINE_KEY, line).assoc(RT.COLUMN_KEY, column);
				}
			if(o instanceof IReference)
				{
				((IReference)o).resetMeta((IPersistentMap) meta);
				return o;
				}
			Object ometa = RT.meta(o);
			for(ISeq s = RT.seq(meta); s != null; s = s.next()) {
			IMapEntry kv = (IMapEntry) s.first();
			ometa = RT.assoc(ometa, kv.getKey(), kv.getValue());
			}
			return ((IObj) o).withMeta((IPersistentMap) ometa);
			}
		else
			throw new IllegalArgumentException("Metadata can only be applied to IMetas");
	}

}

public static class CharacterReader extends AFn{
	public Object invoke(Object reader, Object backslash) {
		PushbackReader r = (PushbackReader) reader;
		int ch = read1(r);
		if(ch == -1)
			throw Util.runtimeException("EOF while reading character");
		String token = readToken(r, (char) ch);
		if(token.length() == 1)
			return Character.valueOf(token.charAt(0));
		else if(token.equals("newline"))
			return '\n';
		else if(token.equals("space"))
			return ' ';
		else if(token.equals("tab"))
			return '\t';
		else if(token.equals("backspace"))
			return '\b';
		else if(token.equals("formfeed"))
			return '\f';
		else if(token.equals("return"))
			return '\r';
		else if(token.startsWith("u"))
			{
			char c = (char) readUnicodeChar(token, 1, 4, 16);
			if(c >= '\uD800' && c <= '\uDFFF') // surrogate code unit?
				throw Util.runtimeException("Invalid character constant: \\u" + Integer.toString(c, 16));
			return c;
			}
		else if(token.startsWith("o"))
			{
			int len = token.length() - 1;
			if(len > 3)
				throw Util.runtimeException("Invalid octal escape sequence length: " + len);
			int uc = readUnicodeChar(token, 1, len, 8);
			if(uc > 0377)
				throw Util.runtimeException("Octal escape sequence must be in range [0, 377].");
			return (char) uc;
			}
		throw Util.runtimeException("Unsupported character: \\" + token);
	}

}

public static class ListReader extends AFn{
	public Object invoke(Object reader, Object leftparen) {
		PushbackReader r = (PushbackReader) reader;
		int line = -1;
		int column = -1;
		if(r instanceof LineNumberingPushbackReader)
			{
			line = ((LineNumberingPushbackReader) r).getLineNumber();
			column = ((LineNumberingPushbackReader) r).getColumnNumber()-1;
			}
		List list = readDelimitedList(')', r, true);
		if(list.isEmpty())
			return PersistentList.EMPTY;
		IObj s = (IObj) PersistentList.create(list);
//		IObj s = (IObj) RT.seq(list);
//		if(line != -1)
//			{
//			return s.withMeta(RT.map(RT.LINE_KEY, line, RT.COLUMN_KEY, column));
//			}
//		else
			return s;
	}

}

public static class VectorReader extends AFn{
	public Object invoke(Object reader, Object leftparen) {
		PushbackReader r = (PushbackReader) reader;
		return LazilyPersistentVector.create(readDelimitedList(']', r, true));
	}

}

public static class MapReader extends AFn{
	public Object invoke(Object reader, Object leftparen) {
		PushbackReader r = (PushbackReader) reader;
		Object[] a = readDelimitedList('}', r, true).toArray();
		if((a.length & 1) == 1)
			throw Util.runtimeException("Map literal must contain an even number of forms");
		return RT.map(a);
	}

}

public static class SetReader extends AFn{
	public Object invoke(Object reader, Object leftbracket) {
		PushbackReader r = (PushbackReader) reader;
		return PersistentHashSet.createWithCheck(readDelimitedList('}', r, true));
	}

}

public static class UnmatchedDelimiterReader extends AFn{
	public Object invoke(Object reader, Object rightdelim) {
		throw Util.runtimeException("Unmatched delimiter: " + rightdelim);
	}

}

public static class UnreadableReader extends AFn{
	public Object invoke(Object reader, Object leftangle) {
		throw Util.runtimeException("Unreadable form");
	}
}

public static List readDelimitedList(char delim, PushbackReader r, boolean isRecursive) {
	final int firstline =
			(r instanceof LineNumberingPushbackReader) ?
			((LineNumberingPushbackReader) r).getLineNumber() : -1;

	ArrayList a = new ArrayList();

	for(; ;)
		{
		int ch = read1(r);

		while(isWhitespace(ch))
			ch = read1(r);

		if(ch == -1)
			{
			if(firstline < 0)
				throw Util.runtimeException("EOF while reading");
			else
				throw Util.runtimeException("EOF while reading, starting at line " + firstline);
			}

		if(ch == delim)
			break;

		IFn macroFn = getMacro(ch);
		if(macroFn != null)
			{
			Object mret = macroFn.invoke(r, (char) ch);
			//no op macros return the reader
			if(mret != r)
				a.add(mret);
			}
		else
			{
			unread(r, ch);

			Object o = read(r, true, null, isRecursive);
			if(o != r)
				a.add(o);
			}
		}


	return a;
}

}

