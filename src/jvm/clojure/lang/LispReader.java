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
import java.util.Map;
import java.math.BigInteger;
import java.lang.*;

public class LispReader{

static Symbol QUOTE = Symbol.create(null, "quote");
//static Symbol SYNTAX_QUOTE = Symbol.create(null, "syntax-quote");
//static Symbol UNQUOTE = Symbol.create(null, "unquote");
//static Symbol UNQUOTE_SPLICING = Symbol.create(null, "unquote-splicing");
static Symbol CONCAT = Symbol.create("clojure", "concat");
static Symbol LIST = Symbol.create("clojure", "list");
static Symbol APPLY = Symbol.create("clojure", "apply");
static Symbol HASHMAP = Symbol.create("clojure", "hash-map");
static Symbol VECTOR = Symbol.create("clojure", "vector");
static Symbol WITH_META = Symbol.create("clojure", "with-meta");
static Symbol META = Symbol.create("clojure", "meta");
static Symbol DEREF = Symbol.create("clojure", "deref");
//static Symbol DEREF_BANG = Symbol.create("clojure", "deref!");

static IFn[] macros = new IFn[256];
static IFn[] dispatchMacros = new IFn[256];
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

//symbol->gensymbol
static Var GENSYM_ENV = Var.create(null);
//sorted-map num->gensymbol
static Var ARG_ENV = Var.create(null);

static
	{
	macros['"'] = new StringReader();
	macros[';'] = new CommentReader();
	macros['\''] = new WrappingReader(Compiler.QUOTE);
	macros['@'] = new WrappingReader(DEREF);//new DerefReader();
	macros['^'] = new WrappingReader(META);
	macros['`'] = new SyntaxQuoteReader();
	macros['~'] = new UnquoteReader();
	macros['('] = new ListReader();
	macros[')'] = new UnmatchedDelimiterReader();
	macros['['] = new VectorReader();
	macros[']'] = new UnmatchedDelimiterReader();
	macros['{'] = new MapReader();
	macros['}'] = new UnmatchedDelimiterReader();
//	macros['|'] = new ArgVectorReader();
	macros['\\'] = new CharacterReader();
	macros['%'] = new ArgReader();
	macros['#'] = new DispatchReader();


	dispatchMacros['^'] = new MetaReader();
	dispatchMacros['\''] = new WrappingReader(Compiler.THE_VAR);
	dispatchMacros['"'] = new RegexReader();
	dispatchMacros['('] = new FnReader();
	}

static boolean isWhitespace(int ch){
	return Character.isWhitespace(ch) || ch == ',';
}

static public Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive)
		throws Exception{

	try
		{
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
	catch(Exception e)
		{
		if(isRecursive || !(r instanceof LineNumberingPushbackReader))
			throw e;
		LineNumberingPushbackReader rdr = (LineNumberingPushbackReader) r;
		throw new Exception(String.format("ReaderError:(%d,1) %s", rdr.getLineNumber(), e.getMessage()), e);
		}
}

static private String readToken(PushbackReader r, char initch) throws Exception{
	StringBuilder sb = new StringBuilder();
	sb.append(initch);

	for(; ;)
		{
		int ch = r.read();
		if(ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch))
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


static private Object interpretToken(String s) throws Exception{
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

	throw new Exception("Invalid token: " + s);
}


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

static private boolean isTerminatingMacro(int ch){
	return (ch != '#' && ch < macros.length && macros[ch] != null);
}

static class RegexReader extends AFn{
	static StringReader stringrdr = new StringReader();
	public Object invoke(Object reader, Object doublequote) throws Exception{
		String str = (String) stringrdr.invoke(reader, doublequote);
		return Pattern.compile(str);
	}
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

static class WrappingReader extends AFn{
	final Symbol sym;

	public WrappingReader(Symbol sym){
		this.sym = sym;
	}

	public Object invoke(Object reader, Object quote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object o = read(r, true, null, true);
		return RT.list(sym, o);
	}

}

/*
static class DerefReader extends AFn{

	public Object invoke(Object reader, Object quote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int ch = r.read();
		if(ch == -1)
			throw new Exception("EOF while reading character");
		if(ch == '!')
			{
			Object o = read(r, true, null, true);
			return RT.list(DEREF_BANG, o);
			}
		else
			{
			r.unread(ch);
			Object o = read(r, true, null, true);
			return RT.list(DEREF, o);
			}
	}

}
*/

static class DispatchReader extends AFn{
	public Object invoke(Object reader, Object hash) throws Exception{
		int ch = ((Reader) reader).read();
		if(ch == -1)
			throw new Exception("EOF while reading character");
		IFn fn = dispatchMacros[ch];
		if(fn == null)
			throw new Exception(String.format("No dispatch macro for: %c", (char) ch));
		return fn.invoke(reader, ch);
	}
}

static Symbol garg(int n){
	return Symbol.intern(null,(n == -1?"rest":("p" + n)) + "__" + RT.nextID());
}

static class FnReader extends AFn{
	public Object invoke(Object reader, Object lparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		try
			{
			Var.pushThreadBindings(
					RT.map(ARG_ENV, PersistentTreeMap.EMPTY));
			r.unread('(');
			Object form = read(r, true, null, true);

			PersistentVector args = PersistentVector.EMPTY;
			PersistentTreeMap argsyms = (PersistentTreeMap) ARG_ENV.get();
			ISeq rargs = argsyms.rseq();
			if(rargs != null)
				{
				int higharg = (Integer)((Map.Entry)rargs.first()).getKey();
				if(higharg > 0)
					{
					for(int i = 1;i<=higharg;++i)
						{
						Object sym = argsyms.valAt(i);
						if(sym == null)
							sym = garg(i);
						args = args.cons(sym);
						}
					}
				Object restsym = argsyms.valAt(-1);
				if(restsym != null)
					{
					args = args.cons(Compiler._AMP_);
					args = args.cons(restsym);
					}
				}
			return RT.list(Compiler.FN,args,form);
			}
		finally
			{
			Var.popThreadBindings();
			}
	}
}

static Symbol registerArg(int n){
	PersistentTreeMap argsyms = (PersistentTreeMap) ARG_ENV.get();
	if(argsyms == null)
		{
		throw new IllegalStateException("arg literal not in #()");
		}
	Symbol ret = (Symbol) argsyms.valAt(n);
	if(ret == null)
		{
		ret =  garg(n);
		ARG_ENV.set(argsyms.assoc(n, ret));
		}
	return ret;
}

static class ArgReader extends AFn{
	public Object invoke(Object reader, Object pct) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int ch = r.read();
		r.unread(ch);
		//% alone is first arg
		if(ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch))
			{
			return registerArg(1);
			}
		Object n = read(r, true, null, true);
		if(n.equals(Compiler._AMP_))
			return registerArg(-1);
		if(!(n instanceof Number))
			throw new IllegalStateException("arg literal must be %, %& or %integer");
		return registerArg(((Number) n).intValue());
	}
}

static class MetaReader extends AFn{
	public Object invoke(Object reader, Object caret) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int line = -1;
		if(r instanceof LineNumberingPushbackReader)
			line = ((LineNumberingPushbackReader) r).getLineNumber();
		Object meta = read(r, true, null, true);
		if(meta instanceof Symbol || meta instanceof Keyword || meta instanceof String)
			meta = RT.map(RT.TAG_KEY, meta);
		else if(!(meta instanceof IPersistentMap))
			throw new IllegalArgumentException("Metadata must be Symbol,Keyword,String or Map");

		Object o = read(r, true, null, true);
		if(o instanceof IObj)
			{
			if(line != -1 && o instanceof ISeq)
				meta = ((IPersistentMap) meta).assoc(RT.LINE_KEY, line);
			return ((IObj) o).withMeta((IPersistentMap) meta);
			}
		else
			throw new IllegalArgumentException("Metadata can only be applied to IObjs");
	}

}

static class SyntaxQuoteReader extends AFn{
	public Object invoke(Object reader, Object backquote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		try
			{
			Var.pushThreadBindings(
					RT.map(GENSYM_ENV, PersistentHashMap.EMPTY));

			Object form = read(r, true, null, true);
			return syntaxQuote(form);
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	static Object syntaxQuote(Object form) throws Exception{
		Object ret;
		if(Compiler.isSpecial(form))
			ret = RT.list(Compiler.QUOTE, form);
		else if(form instanceof Symbol)
			{
			Symbol sym = (Symbol) form;
			if(sym.ns == null && sym.name.endsWith("#"))
				{
				IPersistentMap gmap = (IPersistentMap) GENSYM_ENV.get();
				if(gmap == null)
					throw new IllegalStateException("Gensym literal not in syntax-quote");
				Symbol gs = (Symbol) gmap.valAt(sym);
				if(gs == null)
					GENSYM_ENV.set(gmap.assoc(sym, gs = Symbol.intern(null,
					                                                  sym.name.substring(0, sym.name.length() - 1)
					                                                  + "__" + RT.nextID())));
				sym = gs;
				}
			else
				sym = Compiler.resolveSymbol(sym);
			ret = RT.list(Compiler.QUOTE, sym);
			}
		else if(form instanceof Unquote)
			return ((Unquote) form).o;
		else if(form instanceof UnquoteSplicing)
			throw new IllegalStateException("splice not in list");
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
				ret = RT.cons(CONCAT, sqExpandList(seq));
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
			if(item instanceof Unquote)
				ret = ret.cons(RT.list(LIST, ((Unquote) item).o));
			else if(item instanceof UnquoteSplicing)
				ret = ret.cons(((UnquoteSplicing) item).o);
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

static class Unquote{
	final Object o;

	public Unquote(Object o){
		this.o = o;
	}
}

static class UnquoteSplicing{
	final Object o;

	public UnquoteSplicing(Object o){
		this.o = o;
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
			return new UnquoteSplicing(o);
			}
		else
			{
			r.unread(ch);
			Object o = read(r, true, null, true);
			return new Unquote(o);
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
		int line = -1;
		if(r instanceof LineNumberingPushbackReader)
			line = ((LineNumberingPushbackReader) r).getLineNumber();
		List list = readDelimitedList(')', r, true);
		if(list.isEmpty())
			return PersistentList.EMPTY;
		IObj s = (IObj) PersistentList.create(list);
//		IObj s = (IObj) RT.seq(list);
		if(line != -1)
			return s.withMeta(RT.map(RT.LINE_KEY, line));
		else
			return s;
	}

}

//static class ArgVectorReader extends AFn{
//	public Object invoke(Object reader, Object leftparen) throws Exception{
//		PushbackReader r = (PushbackReader) reader;
//		return ArgVector.create(readDelimitedList('|', r, true));
//	}
//
//}

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

/*
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
 */

}

