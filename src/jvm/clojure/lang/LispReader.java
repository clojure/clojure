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

import java.io.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.ArrayList;
import java.util.List;
import java.math.BigInteger;

public class LispReader{

static Symbol QUOTE = Symbol.create(null, "quote");
//static Symbol SYNTAX_QUOTE = Symbol.create(null, "syntax-quote");
static Symbol UNQUOTE = Symbol.create(null, "unquote");
static Symbol UNQUOTE_SPLICING = Symbol.create(null, "unquote-splicing");
static Symbol CONCAT = Symbol.create("clojure", "concat");
static Symbol LIST = Symbol.create("clojure", "list");
static Symbol APPLY = Symbol.create("clojure", "apply");
static Symbol HASHMAP = Symbol.create("clojure", "hashmap");
static Symbol VECTOR = Symbol.create("clojure", "vector");
static Symbol WITH_META = Symbol.create("clojure", "with-meta");

static IFn[] macros = new IFn[256];
static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^:/]][^:/]*/)?[\\D&&[^:/]][^:/]*");
//static Pattern varPat = Pattern.compile("([\\D&&[^:\\.]][^:\\.]*):([\\D&&[^:\\.]][^:\\.]*)");
static Pattern intPat = Pattern.compile("[-+]?[0-9]+\\.?");
static Pattern ratioPat = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
static Pattern floatPat = Pattern.compile("[-+]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?");
static final Symbol SLASH = Symbol.create("/");
//static Pattern accessorPat = Pattern.compile("\\.[a-zA-Z_]\\w*");
//static Pattern instanceMemberPat = Pattern.compile("\\.([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)");
//static Pattern staticMemberPat = Pattern.compile("([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)");
//static Pattern classNamePat = Pattern.compile("([a-zA-Z_][\\w\\.]*)\\.");

static
	{
	macros['"'] = new StringReader();
	macros[';'] = new CommentReader();
	macros['\''] = new QuoteReader();
	macros['`'] = new SyntaxQuoteReader();
	macros['~'] = new UnquoteReader();
	macros['^'] = new MetaReader();
	macros['('] = new ListReader();
	macros[')'] = new UnmatchedDelimiterReader();
	macros['['] = new VectorReader();
	macros[']'] = new UnmatchedDelimiterReader();
	macros['{'] = new MapReader();
	macros['}'] = new UnmatchedDelimiterReader();
	macros['|'] = new ArgVectorReader();
	macros['\\'] = new CharacterReader();
	}

static boolean isWhitespace(int ch){
	return Character.isWhitespace(ch) || ch == ',';
}

static public Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive)
		throws Exception{

	for(; ;)
		{
		int ch = r.read();

		while(isWhitespace(ch))
			ch = r.read();

		if(ch == -1)
			{
			if(eofIsError)
				throw new Exception("EOF while reading");
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
			int ch2 = r.read();
			if(Character.isDigit(ch2))
				{
				r.unread(ch2);
				Object n = readNumber(r, (char) ch);
				if(RT.suppressRead())
					return null;
				return n;
				}
			r.unread(ch2);
			}

		String token = readToken(r, (char) ch);
		if(RT.suppressRead())
			return null;
		return interpretToken(token);
		}
}

static private String readToken(PushbackReader r, char initch) throws Exception{
	StringBuilder sb = new StringBuilder();
	sb.append(initch);

	for(; ;)
		{
		int ch = r.read();
		if(ch == -1 || isWhitespace(ch) || isMacro(ch))
			{
			r.unread(ch);
			return sb.toString();
			}
		sb.append((char) ch);
		}
}

static private Object readNumber(PushbackReader r, char initch) throws Exception{
	StringBuilder sb = new StringBuilder();
	sb.append(initch);

	for(; ;)
		{
		int ch = r.read();
		if(ch == -1 || isWhitespace(ch) || isMacro(ch))
			{
			r.unread(ch);
			break;
			}
		sb.append((char) ch);
		}

	String s = sb.toString();
	Object n = matchNumber(s);
	if(n == null)
		throw new IllegalArgumentException("Invalid number: " + s);
	return n;
}

/*
static private Object readSymbol(PushbackReader r, char initch) throws Exception {
    StringBuilder sb = new StringBuilder();
    sb.append(initch);

    for(;;)
        {
        int ch = r.read();
        if(ch == -1 || Character.isWhitespace(ch) || isMacro(ch))
            {
            r.unread(ch);
            return Symbol.intern(sb.toString());
            }
        else if(ch == '.')
	        {
	        r.unread(ch);
	        Object ret = Symbol.intern(sb.toString());
	        Object mem = null;
	        while((mem = readMember(r)) != null)
		        {
		        //x.foo ==> (.foo  x)
		        if(mem instanceof Symbol)
			        ret = RT.list(mem, ret);
		        else  //x.foo(y z) ==> (.foo x y z)
			        {
			        ISeq rseq = RT.seq(mem);
			        ret = RT.cons(rseq.first(), RT.cons(ret, RT.rest(rseq)));
			        }
		        }
	        return ret;
	        }
        sb.append((char)ch);
        }
}

static private Object readMember(PushbackReader r) throws Exception {
    StringBuilder sb = new StringBuilder();
	int ch = r.read();
	if(ch != '.')
		{
		r.unread(ch);
		return null;
		}
	sb.append((char)ch);
	boolean first = true;
    for(;;)
        {
        ch = r.read();
        if(ch == '(')
	        {
	        //r.unread(ch);
	        ISeq args = readDelimitedList(')', r, true);
	        return RT.cons(Symbol.intern(sb.toString()),args);
	        }
        else if(ch == -1 || ch == '.' || Character.isWhitespace(ch) || isMacro(ch))
            {
            r.unread(ch);
            return Symbol.intern(sb.toString());
            }
        sb.append((char)ch);
        if((first && !Character.isJavaIdentifierStart(ch))
	        || !Character.isJavaIdentifierPart(ch))
	        throw new IllegalArgumentException("Invalid member name component: " + sb.toString());
        first = false;
        }
}
*/

static private Object interpretToken(String s) throws Exception{
	if(s.equals("nil"))
		{
		return null;
		}

	else if(s.equals("/"))
		{
		return SLASH;
		}
	Object ret = null;

	ret = matchSymbol(s);
	if(ret != null)
		return ret;

	throw new Exception("Invalid token: " + s);
}
/*
private static Object matchHostName(String s) {
    Matcher m = accessorPat.matcher(s);
    if(m.matches())
        return new Accessor(s);
    m = classNamePat.matcher(s);
    if(m.matches())
        return new ClassName(RT.resolveClassNameInContext(m.group(1)));
    m = instanceMemberPat.matcher(s);
    if(m.matches())
        return new InstanceMemberName(RT.resolveClassNameInContext(m.group(1)),m.group(2));
    m = staticMemberPat.matcher(s);
    if(m.matches())
        return new StaticMemberName(RT.resolveClassNameInContext(m.group(1)),m.group(2));

    return null;
}

private static Object matchSymbol(String s) {
    Matcher m = symbolPat.matcher(s);
    if(m.matches())
        return Symbol.intern(s);
    return null;
}
*/

private static Object matchSymbol(String s){
	Matcher m = symbolPat.matcher(s);
	if(m.matches())
		{
		boolean isKeyword = s.charAt(0) == ':';
		Symbol sym = Symbol.intern(s.substring(isKeyword ? 1 : 0));
		if(isKeyword)
			return Keyword.intern(sym);
		return sym;
		}
	return null;
}

/*
private static Object matchVar(String s) throws Exception{
	Matcher m = varPat.matcher(s);
	if(m.matches())
		return Module.intern(m.group(1), m.group(2));
	return null;
}
*/

private static Object matchNumber(String s){
	Matcher m = intPat.matcher(s);
	if(m.matches())
		return Num.from(new BigInteger(s));
	m = floatPat.matcher(s);
	if(m.matches())
		return Num.from(Double.parseDouble(s));
	m = ratioPat.matcher(s);
	if(m.matches())
		{
		return Num.divide(new BigInteger(m.group(1)), new BigInteger(m.group(2)));
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


static class StringReader extends AFn{
	public Object invoke(Object reader, Object doublequote) throws Exception{
		StringBuilder sb = new StringBuilder();
		Reader r = (Reader) reader;

		for(int ch = r.read(); ch != '"'; ch = r.read())
			{
			if(ch == -1)
				throw new Exception("EOF while reading string");
			if(ch == '\\')	//escape
				{
				ch = r.read();
				if(ch == -1)
					throw new Exception("EOF while reading string");
				switch(ch)
					{
					case't':
						ch = '\t';
						break;
					case'r':
						ch = '\r';
						break;
					case'n':
						ch = '\n';
						break;
					case'\\':
						break;
					case'"':
						break;
					default:
						throw new Exception("Unsupported escape character: \\" + (char) ch);
					}
				}
			sb.append((char) ch);
			}
		return sb.toString();
	}
}

static class CommentReader extends AFn{
	public Object invoke(Object reader, Object semicolon) throws Exception{
		Reader r = (Reader) reader;
		int ch;
		do
			{
			ch = r.read();
			} while(ch != -1 && ch != '\n' && ch != '\r');
		return r;
	}

}

static class QuoteReader extends AFn{
	public Object invoke(Object reader, Object quote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object o = read(r, true, null, true);
		return RT.list(Compiler.QUOTE, o);
	}

}

static class MetaReader extends AFn{
	public Object invoke(Object reader, Object caret) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object meta = read(r, true, null, true);
		if(meta instanceof Symbol || meta instanceof Keyword)
			meta = RT.map(RT.TAG_KEY, meta);
		else if(!(meta instanceof IPersistentMap))
			throw new IllegalArgumentException("Metadata must be Symbol,Keyword or Map");
		Object o = read(r, true, null, true);
		if(o instanceof IObj)
			return ((IObj) o).withMeta((IPersistentMap) meta);
		else
			throw new IllegalArgumentException("Metadata can only be applied to IObjs");
	}

}

static class SyntaxQuoteReader extends AFn{
	public Object invoke(Object reader, Object backquote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object form = read(r, true, null, true);

		return syntaxQuote(form);
	}

	static Object syntaxQuote(Object form) throws Exception{
		Object ret;
		if(Compiler.isSpecial(form))
			ret = RT.list(Compiler.QUOTE, form);
		else if(form instanceof Symbol)
			ret = RT.list(Compiler.QUOTE, Compiler.resolveSymbol((Symbol) form));
		else if(form instanceof IPersistentCollection)
			{
			if(form instanceof IPersistentMap)
				{
				IPersistentVector keyvals = flattenMap(form);
				ret = RT.list(APPLY, HASHMAP, RT.cons(CONCAT, sqExpandList(keyvals.seq())));
				}
			else if(form instanceof IPersistentVector)
				{
				ret = RT.list(APPLY, VECTOR, RT.cons(CONCAT, sqExpandList(((IPersistentVector) form).seq())));
				}
			else if(form instanceof ISeq)
				{
				ISeq seq = RT.seq(form);
				if(RT.equal(UNQUOTE, RT.first(seq)))
					return RT.second(seq);
				else if(RT.equal(UNQUOTE_SPLICING, RT.first(seq)))
					throw new IllegalStateException("splice not in list");
				else
					{
					ret = RT.cons(CONCAT, sqExpandList(seq));
					}
				}
			else
				throw new UnsupportedOperationException("Unknown Collection type");
			}
		else if(form instanceof Keyword
		        || form instanceof Num
		        || form instanceof Character
		        || form instanceof String)
			ret = form;
		else
			ret = RT.list(Compiler.QUOTE, form);

		if(form instanceof IObj && ((IObj) form).meta() != null)
			return RT.list(WITH_META, ret, syntaxQuote(((IObj) form).meta()));
		else
			return ret;
	}

	private static ISeq sqExpandList(ISeq seq) throws Exception{
		PersistentVector ret = PersistentVector.EMPTY;
		for(; seq != null; seq = seq.rest())
			{
			Object item = seq.first();
			if(item instanceof ISeq && RT.equal(UNQUOTE, RT.first(item)))
				ret = ret.cons(RT.list(LIST, RT.second(item)));
			else if(item instanceof ISeq && RT.equal(UNQUOTE_SPLICING, RT.first(item)))
				ret = ret.cons(RT.second(item));
			else
				ret = ret.cons(RT.list(LIST, syntaxQuote(item)));
			}
		return ret.seq();
	}

	private static IPersistentVector flattenMap(Object form){
		IPersistentVector keyvals = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form); s != null; s = s.rest())
			{
			IMapEntry e = (IMapEntry) s.first();
			keyvals = (IPersistentVector) keyvals.cons(e.key());
			keyvals = (IPersistentVector) keyvals.cons(e.val());
			}
		return keyvals;
	}

}


static class UnquoteReader extends AFn{
	public Object invoke(Object reader, Object comma) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int ch = r.read();
		if(ch == -1)
			throw new Exception("EOF while reading character");
		if(ch == '@')
			{
			Object o = read(r, true, null, true);
			return RT.list(Compiler.UNQUOTE_SPLICING, o);
			}
		else
			{
			r.unread(ch);
			Object o = read(r, true, null, true);
			return RT.list(Compiler.UNQUOTE, o);
			}
	}

}

static class CharacterReader extends AFn{
	public Object invoke(Object reader, Object backslash) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int ch = r.read();
		if(ch == -1)
			throw new Exception("EOF while reading character");
		String token = readToken(r, (char) ch);
		if(token.length() == 1)
			return Character.valueOf(token.charAt(0));
		else if(token.equals("newline"))
			return '\n';
		else if(token.equals("space"))
			return ' ';
		else if(token.equals("tab"))
			return '\t';
		throw new Exception("Unsupported character: \\" + token);
	}

}

static class ListReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		List list = readDelimitedList(')', r, true);
		if(list.isEmpty())
			return PersistentList.EMPTY;
		return RT.seq(list);
	}

}

static class ArgVectorReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		return ArgVector.create(readDelimitedList('|', r, true));
	}

}

static class VectorReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		return PersistentVector.create(readDelimitedList(']', r, true));
	}

}

static class MapReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		return PersistentHashMap.create(readDelimitedList('}', r, true));
	}

}

static class UnmatchedDelimiterReader extends AFn{
	public Object invoke(Object reader, Object rightdelim) throws Exception{
		throw new Exception("Unmatched delimiter: " + rightdelim);
	}

}

public static List readDelimitedList(char delim, PushbackReader r, boolean isRecursive) throws Exception{
	ArrayList a = new ArrayList();

	for(; ;)
		{
		int ch = r.read();

		while(isWhitespace(ch))
			ch = r.read();

		if(ch == -1)
			throw new Exception("EOF while reading");

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
			r.unread(ch);

			Object o = read(r, true, null, isRecursive);
			if(o != r)
				a.add(o);
			}
		}


	return a;
}

public static void main(String[] args){
	LineNumberingPushbackReader r = new LineNumberingPushbackReader(new InputStreamReader(System.in));
	OutputStreamWriter w = new OutputStreamWriter(System.out);
	Object ret = null;
	try
		{
		for(; ;)
			{
			ret = LispReader.read(r, true, null, false);
			RT.print(ret, w);
			w.write('\n');
			if(ret != null)
				w.write(ret.getClass().toString());
			w.write('\n');
			w.flush();
			}
		}
	catch(Exception e)
		{
		e.printStackTrace();
		}
}


}

