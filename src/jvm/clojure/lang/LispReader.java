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

import java.io.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.math.BigInteger;
import java.math.BigDecimal;
import java.lang.*;

public class LispReader{

static final Symbol QUOTE = Symbol.intern("quote");
static final Symbol THE_VAR = Symbol.intern("var");
//static Symbol SYNTAX_QUOTE = Symbol.intern(null, "syntax-quote");
static Symbol UNQUOTE = Symbol.intern("clojure.core", "unquote");
static Symbol UNQUOTE_SPLICING = Symbol.intern("clojure.core", "unquote-splicing");
static Symbol CONCAT = Symbol.intern("clojure.core", "concat");
static Symbol SEQ = Symbol.intern("clojure.core", "seq");
static Symbol LIST = Symbol.intern("clojure.core", "list");
static Symbol APPLY = Symbol.intern("clojure.core", "apply");
static Symbol HASHMAP = Symbol.intern("clojure.core", "hash-map");
static Symbol HASHSET = Symbol.intern("clojure.core", "hash-set");
static Symbol VECTOR = Symbol.intern("clojure.core", "vector");
static Symbol WITH_META = Symbol.intern("clojure.core", "with-meta");
static Symbol META = Symbol.intern("clojure.core", "meta");
static Symbol DEREF = Symbol.intern("clojure.core", "deref");
//static Symbol DEREF_BANG = Symbol.intern("clojure.core", "deref!");

static IFn[] macros = new IFn[256];
static IFn[] dispatchMacros = new IFn[256];
//static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^:/]][^:/]*/)?[\\D&&[^:/]][^:/]*");
static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^/]].*/)?([\\D&&[^/]][^/]*)");
//static Pattern varPat = Pattern.compile("([\\D&&[^:\\.]][^:\\.]*):([\\D&&[^:\\.]][^:\\.]*)");
//static Pattern intPat = Pattern.compile("[-+]?[0-9]+\\.?");
static Pattern intPat =
		Pattern.compile(
				"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
static Pattern ratioPat = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
static Pattern floatPat = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");
static final Symbol SLASH = Symbol.intern("/");
static final Symbol CLOJURE_SLASH = Symbol.intern("clojure.core","/");
//static Pattern accessorPat = Pattern.compile("\\.[a-zA-Z_]\\w*");
//static Pattern instanceMemberPat = Pattern.compile("\\.([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)");
//static Pattern staticMemberPat = Pattern.compile("([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)");
//static Pattern classNamePat = Pattern.compile("([a-zA-Z_][\\w\\.]*)\\.");

//symbol->gensymbol
static Var GENSYM_ENV = Var.create(null).setDynamic();
//sorted-map num->gensymbol
static Var ARG_ENV = Var.create(null).setDynamic();

    static
	{
	macros['"'] = new StringReader();
	macros[';'] = new CommentReader();
	macros['\''] = new WrappingReader(QUOTE);
	macros['@'] = new WrappingReader(DEREF);//new DerefReader();
	macros['^'] = new MetaReader();
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
	dispatchMacros['\''] = new VarReader();
	dispatchMacros['"'] = new RegexReader();
	dispatchMacros['('] = new FnReader();
	dispatchMacros['{'] = new SetReader();
	dispatchMacros['='] = new EvalReader();
	dispatchMacros['!'] = new CommentReader();
	dispatchMacros['<'] = new UnreadableReader();
	dispatchMacros['_'] = new DiscardReader();
	}

static boolean isWhitespace(int ch){
	return Character.isWhitespace(ch) || ch == ',';
}

static void unread(PushbackReader r, int ch) throws IOException{
	if(ch != -1)
		r.unread(ch);
}

public static class ReaderException extends Exception{
	final int line;

	public ReaderException(int line, Throwable cause){
		super(cause);
		this.line = line;
	}
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
			throw e;
		LineNumberingPushbackReader rdr = (LineNumberingPushbackReader) r;
		//throw new Exception(String.format("ReaderError:(%d,1) %s", rdr.getLineNumber(), e.getMessage()), e);
		throw new ReaderException(rdr.getLineNumber(), e);
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
			unread(r, ch);
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

static private int readUnicodeChar(String token, int offset, int length, int base) throws Exception{
	if(token.length() != offset + length)
		throw new IllegalArgumentException("Invalid unicode character: \\" + token);
	int uc = 0;
	for(int i = offset; i < offset + length; ++i)
		{
		int d = Character.digit(token.charAt(i), base);
		if(d == -1)
			throw new IllegalArgumentException("Invalid digit: " + (char) d);
		uc = uc * base + d;
		}
	return (char) uc;
}

static private int readUnicodeChar(PushbackReader r, int initch, int base, int length, boolean exact) throws Exception{
	int uc = Character.digit(initch, base);
	if(uc == -1)
		throw new IllegalArgumentException("Invalid digit: " + initch);
	int i = 1;
	for(; i < length; ++i)
		{
		int ch = r.read();
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
	else if(s.equals("clojure.core//"))
		{
		return CLOJURE_SLASH;
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
		int gc = m.groupCount();
		String ns = m.group(1);
		String name = m.group(2);
		if(ns != null && ns.endsWith(":/")
		   || name.endsWith(":")
		   || s.indexOf("::", 1) != -1)
			return null;
		if(s.startsWith("::"))
			{
			Symbol ks = Symbol.intern(s.substring(2));
			Namespace kns;
			if(ks.ns != null)
				kns = Compiler.namespaceFor(ks);
			else
				kns = Compiler.currentNS();
			//auto-resolving keyword
            if (kns != null)
			    return Keyword.intern(kns.name.name,ks.name);
            else
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
		return Numbers.divide(Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(m.group(1)))),
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

public static class RegexReader extends AFn{
	static StringReader stringrdr = new StringReader();

	public Object invoke(Object reader, Object doublequote) throws Exception{
		StringBuilder sb = new StringBuilder();
		Reader r = (Reader) reader;
		for(int ch = r.read(); ch != '"'; ch = r.read())
			{
			if(ch == -1)
				throw new Exception("EOF while reading regex");
			sb.append( (char) ch );
			if(ch == '\\')	//escape
				{
				ch = r.read();
				if(ch == -1)
					throw new Exception("EOF while reading regex");
				sb.append( (char) ch ) ;
				}
			}
		return Pattern.compile(sb.toString());
	}
}

public static class StringReader extends AFn{
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
					ch = r.read();
					if (Character.digit(ch, 16) == -1)
					    throw new Exception("Invalid unicode escape: \\u" + (char) ch);
					ch = readUnicodeChar((PushbackReader) r, ch, 16, 4, true);
					break;
					}
					default:
					{
					if(Character.isDigit(ch))
						{
						ch = readUnicodeChar((PushbackReader) r, ch, 8, 3, false);
						if(ch > 0377)
							throw new Exception("Octal escape sequence must be in range [0, 377].");
						}
					else
						throw new Exception("Unsupported escape character: \\" + (char) ch);
					}
					}
				}
			sb.append((char) ch);
			}
		return sb.toString();
	}
}

public static class CommentReader extends AFn{
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

public static class DiscardReader extends AFn{
	public Object invoke(Object reader, Object underscore) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		read(r, true, null, true);
		return r;
	}
}

public static class WrappingReader extends AFn{
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

public static class DeprecatedWrappingReader extends AFn{
	final Symbol sym;
        final String macro;

	public DeprecatedWrappingReader(Symbol sym, String macro){
		this.sym = sym;
                this.macro = macro;
	}

	public Object invoke(Object reader, Object quote) throws Exception{
                System.out.println("WARNING: reader macro " + macro +
                                   " is deprecated; use " + sym.getName() +
                                   " instead");
		PushbackReader r = (PushbackReader) reader;
		Object o = read(r, true, null, true);
		return RT.list(sym, o);
	}

}

public static class VarReader extends AFn{
	public Object invoke(Object reader, Object quote) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object o = read(r, true, null, true);
//		if(o instanceof Symbol)
//			{
//			Object v = Compiler.maybeResolveIn(Compiler.currentNS(), (Symbol) o);
//			if(v instanceof Var)
//				return v;
//			}
		return RT.list(THE_VAR, o);
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

public static class DispatchReader extends AFn{
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
	return Symbol.intern(null, (n == -1 ? "rest" : ("p" + n)) + "__" + RT.nextID() + "#");
}

public static class FnReader extends AFn{
	public Object invoke(Object reader, Object lparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		if(ARG_ENV.deref() != null)
			throw new IllegalStateException("Nested #()s are not allowed");
		try
			{
			Var.pushThreadBindings(
					RT.map(ARG_ENV, PersistentTreeMap.EMPTY));
			r.unread('(');
			Object form = read(r, true, null, true);

			PersistentVector args = PersistentVector.EMPTY;
			PersistentTreeMap argsyms = (PersistentTreeMap) ARG_ENV.deref();
			ISeq rargs = argsyms.rseq();
			if(rargs != null)
				{
				int higharg = (Integer) ((Map.Entry) rargs.first()).getKey();
				if(higharg > 0)
					{
					for(int i = 1; i <= higharg; ++i)
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
			return RT.list(Compiler.FN, args, form);
			}
		finally
			{
			Var.popThreadBindings();
			}
	}
}

static Symbol registerArg(int n){
	PersistentTreeMap argsyms = (PersistentTreeMap) ARG_ENV.deref();
	if(argsyms == null)
		{
		throw new IllegalStateException("arg literal not in #()");
		}
	Symbol ret = (Symbol) argsyms.valAt(n);
	if(ret == null)
		{
		ret = garg(n);
		ARG_ENV.set(argsyms.assoc(n, ret));
		}
	return ret;
}

static class ArgReader extends AFn{
	public Object invoke(Object reader, Object pct) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		if(ARG_ENV.deref() == null)
			{
			return interpretToken(readToken(r, '%'));
			}
		int ch = r.read();
		unread(r, ch);
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

public static class MetaReader extends AFn{
	public Object invoke(Object reader, Object caret) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		int line = -1;
		if(r instanceof LineNumberingPushbackReader)
			line = ((LineNumberingPushbackReader) r).getLineNumber();
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
				meta = ((IPersistentMap) meta).assoc(RT.LINE_KEY, line);
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

public static class SyntaxQuoteReader extends AFn{
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
				IPersistentMap gmap = (IPersistentMap) GENSYM_ENV.deref();
				if(gmap == null)
					throw new IllegalStateException("Gensym literal not in syntax-quote");
				Symbol gs = (Symbol) gmap.valAt(sym);
				if(gs == null)
					GENSYM_ENV.set(gmap.assoc(sym, gs = Symbol.intern(null,
					                                                  sym.name.substring(0, sym.name.length() - 1)
					                                                  + "__" + RT.nextID() + "__auto__")));
				sym = gs;
				}
			else if(sym.ns == null && sym.name.endsWith("."))
				{
				Symbol csym = Symbol.intern(null, sym.name.substring(0, sym.name.length() - 1));
				csym = Compiler.resolveSymbol(csym);
				sym = Symbol.intern(null, csym.name.concat("."));
				}
			else if(sym.ns == null && sym.name.startsWith("."))
				{
				// Simply quote method names.
 				}
            else
				{
					Object maybeClass = null;
					if(sym.ns != null)
						maybeClass = Compiler.currentNS().getMapping(
								Symbol.intern(null, sym.ns));
					if(maybeClass instanceof Class)
						{
						// Classname/foo -> package.qualified.Classname/foo
						sym = Symbol.intern(
								((Class)maybeClass).getName(), sym.name);
						}
					else
						sym = Compiler.resolveSymbol(sym);
				}
			ret = RT.list(Compiler.QUOTE, sym);
			}
		else if(isUnquote(form))
			return RT.second(form);
		else if(isUnquoteSplicing(form))
			throw new IllegalStateException("splice not in list");
		else if(form instanceof IPersistentCollection)
			{
			if(form instanceof IPersistentMap)
				{
				IPersistentVector keyvals = flattenMap(form);
                ret = RT.list(APPLY, HASHMAP, RT.list(SEQ, RT.cons(CONCAT, sqExpandList(keyvals.seq()))));
				}
			else if(form instanceof IPersistentVector)
                {
                ret = RT.list(APPLY, VECTOR, RT.list(SEQ, RT.cons(CONCAT, sqExpandList(((IPersistentVector) form).seq()))));
                }
			else if(form instanceof IPersistentSet)
                    {
                    ret = RT.list(APPLY, HASHSET, RT.list(SEQ, RT.cons(CONCAT, sqExpandList(((IPersistentSet) form).seq()))));
                    }
			else if(form instanceof ISeq || form instanceof IPersistentList)
				{
				ISeq seq = RT.seq(form);
                if(seq == null)
                    ret = RT.cons(LIST,null);
                else
                    ret = RT.list(SEQ, RT.cons(CONCAT, sqExpandList(seq)));
				}
			else
				throw new UnsupportedOperationException("Unknown Collection type");
			}
		else if(form instanceof Keyword
		        || form instanceof Number
		        || form instanceof Character
		        || form instanceof String)
			ret = form;
		else
			ret = RT.list(Compiler.QUOTE, form);

		if(form instanceof IObj && RT.meta(form) != null)
			{
			//filter line numbers
			IPersistentMap newMeta = ((IObj) form).meta().without(RT.LINE_KEY);
			if(newMeta.count() > 0)
				return RT.list(WITH_META, ret, syntaxQuote(((IObj) form).meta()));
			}
		return ret;
	}

	private static ISeq sqExpandList(ISeq seq) throws Exception{
		PersistentVector ret = PersistentVector.EMPTY;
		for(; seq != null; seq = seq.next())
			{
			Object item = seq.first();
			if(isUnquote(item))
				ret = ret.cons(RT.list(LIST, RT.second(item)));
			else if(isUnquoteSplicing(item))
				ret = ret.cons(RT.second(item));
			else
				ret = ret.cons(RT.list(LIST, syntaxQuote(item)));
			}
		return ret.seq();
	}

	private static IPersistentVector flattenMap(Object form){
		IPersistentVector keyvals = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form); s != null; s = s.next())
			{
			IMapEntry e = (IMapEntry) s.first();
			keyvals = (IPersistentVector) keyvals.cons(e.key());
			keyvals = (IPersistentVector) keyvals.cons(e.val());
			}
		return keyvals;
	}

}

static boolean isUnquoteSplicing(Object form){
	return form instanceof ISeq && Util.equals(RT.first(form),UNQUOTE_SPLICING);
}

static boolean isUnquote(Object form){
	return form instanceof ISeq && Util.equals(RT.first(form),UNQUOTE);
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
			return RT.list(UNQUOTE_SPLICING, o);
			}
		else
			{
			unread(r, ch);
			Object o = read(r, true, null, true);
			return RT.list(UNQUOTE, o);
			}
	}

}

public static class CharacterReader extends AFn{
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
			     throw new Exception("Invalid character constant: \\u" + Integer.toString(c, 16));
			 return c;
		    }
		else if(token.startsWith("o"))
			{
			int len = token.length() - 1;
			if(len > 3)
				throw new Exception("Invalid octal escape sequence length: " + len);
			int uc = readUnicodeChar(token, 1, len, 8);
			if(uc > 0377)
				throw new Exception("Octal escape sequence must be in range [0, 377].");
			return (char) uc;
			}
		throw new Exception("Unsupported character: \\" + token);
	}

}

public static class ListReader extends AFn{
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

static class CtorReader extends AFn{
	static final Symbol cls = Symbol.intern("class");

	public Object invoke(Object reader, Object leftangle) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		// #<class classname>
		// #<classname args*>
		// #<classname/staticMethod args*>
		List list = readDelimitedList('>', r, true);
		if(list.isEmpty())
			throw new Exception("Must supply 'class', classname or classname/staticMethod");
		Symbol s = (Symbol) list.get(0);
		Object[] args = list.subList(1, list.size()).toArray();
		if(s.equals(cls))
			{
			return RT.classForName(args[0].toString());
			}
		else if(s.ns != null) //static method
			{
			String classname = s.ns;
			String method = s.name;
			return Reflector.invokeStaticMethod(classname, method, args);
			}
		else
			{
			return Reflector.invokeConstructor(RT.classForName(s.name), args);
			}
	}

}

public static class EvalReader extends AFn{
	public Object invoke(Object reader, Object eq) throws Exception{
		if (!RT.booleanCast(RT.READEVAL.deref()))
	    {
		  throw new Exception("EvalReader not allowed when *read-eval* is false.");
	    }
		
		PushbackReader r = (PushbackReader) reader;
		Object o = read(r, true, null, true);
		if(o instanceof Symbol)
			{
			return RT.classForName(o.toString());
			}
		else if(o instanceof IPersistentList)
			{
			Symbol fs = (Symbol) RT.first(o);
			if(fs.equals(THE_VAR))
				{
				Symbol vs = (Symbol) RT.second(o);
				return RT.var(vs.ns, vs.name);  //Compiler.resolve((Symbol) RT.second(o),true);
				}
			if(fs.name.endsWith("."))
				{
				Object[] args = RT.toArray(RT.next(o));
				return Reflector.invokeConstructor(RT.classForName(fs.name.substring(0, fs.name.length() - 1)), args);
				}
			if(Compiler.namesStaticMember(fs))
				{
				Object[] args = RT.toArray(RT.next(o));
				return Reflector.invokeStaticMethod(fs.ns, fs.name, args);
				}
			Object v = Compiler.maybeResolveIn(Compiler.currentNS(), fs);
			if(v instanceof Var)
				{
				return ((IFn) v).applyTo(RT.next(o));
				}
			throw new Exception("Can't resolve " + fs);
			}
		else
			throw new IllegalArgumentException("Unsupported #= form");
	}
}

//static class ArgVectorReader extends AFn{
//	public Object invoke(Object reader, Object leftparen) throws Exception{
//		PushbackReader r = (PushbackReader) reader;
//		return ArgVector.create(readDelimitedList('|', r, true));
//	}
//
//}

public static class VectorReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		return LazilyPersistentVector.create(readDelimitedList(']', r, true));
	}

}

public static class MapReader extends AFn{
	public Object invoke(Object reader, Object leftparen) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		Object[] a = readDelimitedList('}', r, true).toArray();
		if((a.length & 1) == 1)
			throw new Exception("Map literal must contain an even number of forms");
		return RT.map(a);
	}

}

public static class SetReader extends AFn{
	public Object invoke(Object reader, Object leftbracket) throws Exception{
		PushbackReader r = (PushbackReader) reader;
		return PersistentHashSet.createWithCheck(readDelimitedList('}', r, true));
	}

}

public static class UnmatchedDelimiterReader extends AFn{
	public Object invoke(Object reader, Object rightdelim) throws Exception{
		throw new Exception("Unmatched delimiter: " + rightdelim);
	}

}

public static class UnreadableReader extends AFn{
	public Object invoke(Object reader, Object leftangle) throws Exception{
		throw new Exception("Unreadable form");
	}
}

public static List readDelimitedList(char delim, PushbackReader r, boolean isRecursive) throws Exception{
	final int firstline =
		(r instanceof LineNumberingPushbackReader) ?
		((LineNumberingPushbackReader) r).getLineNumber() : -1;

	ArrayList a = new ArrayList();

	for(; ;)
		{
		int ch = r.read();

		while(isWhitespace(ch))
			ch = r.read();

		if(ch == -1)
			{
			if(firstline < 0)
				throw new Exception("EOF while reading");
			else
				throw new Exception("EOF while reading, starting at line " + firstline);
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

/*
public static void main(String[] args) throws Exception{
	//RT.init();
	PushbackReader rdr = new PushbackReader( new java.io.StringReader( "(+ 21 21)" ) );
	Object input = LispReader.read(rdr, false, new Object(), false );
	System.out.println(Compiler.eval(input));
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
 */

}

