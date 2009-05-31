/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using java.math;
using System.IO;
using System.Collections;
using clojure.runtime;

namespace clojure.lang
{
    /// <summary>
    /// Implements the Lisp reader, a marvel to behold.
    /// </summary>
    public class LispReader
    {
        #region Symbol definitions

        static readonly Symbol QUOTE = Symbol.create("quote");
        static readonly Symbol THE_VAR = Symbol.create("var");
        static readonly Symbol UNQUOTE = Symbol.create("clojure.core", "unquote");
        static readonly Symbol UNQUOTE_SPLICING = Symbol.create("clojure.core", "unquote-splicing");
        static readonly Symbol DEREF = Symbol.create("clojure.core", "deref");
        static readonly Symbol META = Symbol.create("clojure.core", "meta");
        static readonly Symbol APPLY = Symbol.create("clojure.core", "apply");
        static readonly Symbol CONCAT = Symbol.create("clojure.core", "concat");
        static readonly Symbol HASHMAP = Symbol.create("clojure.core", "hash-map");
        static readonly Symbol HASHSET = Symbol.create("clojure.core", "hash-set");
        static readonly Symbol VECTOR = Symbol.create("clojure.core", "vector");
        static readonly Symbol WITH_META = Symbol.create("clojure.core", "with-meta");
        static readonly Symbol LIST = Symbol.create("clojure.core", "list");
        static readonly Symbol SEQ = Symbol.create("clojure.core","seq");

        static readonly Symbol SLASH = Symbol.create("/");
        static readonly Symbol CLOJURE_SLASH = Symbol.create("clojure.core","/");

        #endregion

        #region Var environments

        //symbol->gensymbol
        /// <summary>
        /// Dynamically bound var to a map from <see cref="Symbol">Symbol</see>s to ...
        /// </summary>
        static Var GENSYM_ENV = Var.create(null);

        //sorted-map num->gensymbol
        static Var ARG_ENV = Var.create(null);
        
        #endregion

        #region Macro characters & #-dispatch

        static IFn[] _macros = new IFn[256];
        static IFn[] _dispatchMacros = new IFn[256];

        static LispReader()
        {
            _macros['"'] = new StringReader();
            _macros[';'] = new CommentReader();
            _macros['\''] = new WrappingReader(QUOTE);
            _macros['@'] = new WrappingReader(DEREF);//new DerefReader();
            _macros['^'] = new WrappingReader(META);
            _macros['`'] = new SyntaxQuoteReader();
            _macros['~'] = new UnquoteReader();
            _macros['('] = new ListReader();
            _macros[')'] = new UnmatchedDelimiterReader();
            _macros['['] = new VectorReader();
            _macros[']'] = new UnmatchedDelimiterReader();
            _macros['{'] = new MapReader();
            _macros['}'] = new UnmatchedDelimiterReader();
            ////	macros['|'] = new ArgVectorReader();            
            _macros['\\'] = new CharacterReader();
            _macros['%'] = new ArgReader();
            _macros['#'] = new DispatchReader();


            _dispatchMacros['^'] = new MetaReader();
            _dispatchMacros['\''] = new VarReader();
            _dispatchMacros['"'] = new RegexReader();
            _dispatchMacros['('] = new FnReader();
            _dispatchMacros['{'] = new SetReader();
            _dispatchMacros['='] = new EvalReader();
            _dispatchMacros['!'] = new CommentReader();
            _dispatchMacros['<'] = new UnreadableReader();
            _dispatchMacros['_'] = new DiscardReader();
        }

        static bool isMacro(int ch)
        {
            return ch < _macros.Length && _macros[ch] != null;
        }

        static IFn getMacro(int ch)
        {
            return ch < _macros.Length ? _macros[ch] : null;
        }

        static bool isTerminatingMacro(int ch)
        {
            return (ch != '#' && ch < _macros.Length && _macros[ch] != null);
        }

        #endregion

        #region main entry point -- read

        // There is really no reason for the main entry point to have an isRecursive flag, is there?

        public static object read(PushbackTextReader r,
            bool eofIsError,
            object eofValue,
            bool isRecursive)
        {
            try
            {
                for (; ; )
                {
                    int ch = r.Read();

                    while (isWhitespace(ch))
                        ch = r.Read();

                    if (ch == -1)
                    {
                        if (eofIsError)
                            throw new EndOfStreamException("EOF while reading");
                        return eofValue;
                    }

                    if (Char.IsDigit((char)ch))
                    {
                        object n = readNumber(r, (char)ch);
                        return RT.suppressRead() ? null : n;
                    }

                    IFn macroFn = getMacro(ch);
                    if (macroFn != null)
                    {
                        object ret = macroFn.invoke(r, (char)ch);
                        if (RT.suppressRead())
                            return null;
                        // no op macros return the reader
                        if (ret == r)
                            continue;
                        return ret;
                    }

                    if (ch == '+' || ch == '-')
                    {
                        int ch2 = r.Read();
                        if (Char.IsDigit((char)ch2))
                        {
                            Unread(r, ch2);
                            object n = readNumber(r, (char)ch);
                            return RT.suppressRead() ? null : n;
                        }
                        Unread(r, ch2);
                    }

                    string token = readToken(r, (char)ch);
                    return RT.suppressRead() ? null : interpretToken(token);
                }
            }
            catch (Exception e)
            {
                if (isRecursive || !(r is LineNumberingTextReader))
                    throw e;
                LineNumberingTextReader rdr = r as LineNumberingTextReader;
                throw new ReaderException(rdr.LineNumber, e);
            }
        }

        private static object ReadAux(PushbackTextReader r)
        {
            return read(r, true, null, true);
        }

        #endregion
        
        #region Character hacking

        static void Unread(PushbackTextReader r, int ch)
        {
            if (ch != -1)
                r.Unread(ch);
        }


        static bool isWhitespace(int ch)
        {
            return Char.IsWhiteSpace((char)ch) || ch == ',';
        }




        // Roughly a match to Java Character.digit(char,int),
        // though I don't handle all unicode digits.
        static int CharValueInRadix(int c, int radix)
        {
            if (char.IsDigit((char)c))
                return c - '0' < radix ? c - '0' : -1;

            if ('A' <= c && c <= 'Z')
                return c - 'A' < radix - 10 ? c - 'A' + 10: -1;

            if ('a' <= c && c <= 'z')
                return c - 'a' < radix - 10 ? c - 'a' + 10 : -1;

            return -1;
        }

        static int readUnicodeChar(string token, int offset, int length, int radix)
        {
            if (token.Length != offset + length)
                throw new ArgumentException("Invalid unicode character: \\" + token);
            int uc = 0;
            for (int i = offset; i < offset + length; ++i)
            {
                int d = CharValueInRadix(token[i], radix);
                if (d == -1)
                    throw new ArgumentException("Invalid digit: " + (char)d);
                uc = uc * radix + d;
            }
            return (char)uc;
        }

        static int readUnicodeChar(PushbackTextReader r, int initch, int radix, int length, bool exact)
        {

            int uc = CharValueInRadix(initch, radix);
            if (uc == -1)
                throw new ArgumentException("Invalid digit: " + initch);
            int i = 1;
            for (; i < length; ++i)
            {
                int ch = r.Read();
                if (ch == -1 || isWhitespace(ch) || isMacro(ch))
                {
                    Unread(r, ch);
                    break;
                }
                int d = CharValueInRadix(ch, radix);
                if (d == -1)
                    throw new ArgumentException("Invalid digit: " + (char)ch);
                uc = uc * radix + d;
            }
            if (i != length && exact)
                throw new ArgumentException("Invalid character length: " + i + ", should be: " + length);
            return uc;
        }

        #endregion

        #region  Other 
        
        public static List<Object> readDelimitedList(char delim, PushbackTextReader r, bool isRecursive)
        {
            List<Object> a = new List<object>();

            for (; ; )
            {
                int ch = r.Read();

                while (isWhitespace(ch))
                    ch = r.Read();

                if (ch == -1)
                    throw new EndOfStreamException("EOF while reading");

                if (ch == delim)
                {
                    break;
                }

                IFn macroFn = getMacro(ch);
                if (macroFn != null)
                {
                    Object mret = macroFn.invoke(r, (char)ch);
                    //no op macros return the reader
                    if (mret != r)
                        a.Add(mret);
                }
                else
                {
                    Unread(r, ch);
                    object o = read(r, true, null, isRecursive);
                    if (o != r)
                        a.Add(o);
                }
            }

            return a;
        }

        static Symbol garg(int n)
        {
            return Symbol.intern(null, (n == -1 ? "rest" : ("p" + n)) + "__" + RT.nextID());
        }


        #endregion

        #region Reading tokens

        static string readToken(PushbackTextReader r, char initch) 
        {
	        StringBuilder sb = new StringBuilder();
            sb.Append(initch);
            
            for(; ;)
            {
                int ch = r.Read();
                if(ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch))
                {
                    Unread(r, ch);
                    return sb.ToString();
                }
                sb.Append((char) ch);
            }
        }

        public static object interpretToken(string s)
        {

            if (s.Equals("nil"))
            {
                return null;
            }
            else if (s.Equals("true"))
            {
                return RT.T;
            }
            else if (s.Equals("false"))
            {
                return RT.F;
            }
            else if (s.Equals("/"))
            {
                return SLASH;
            }
            else if (s.Equals("clojure.core//"))
            {
                return CLOJURE_SLASH;
            }

            object ret = null;

            ret = matchSymbol(s);
            if (ret != null)
                return ret;

            throw new Exception("Invalid token: " + s);
        }


        static Regex symbolPat = new Regex("^[:]?([\\D-[/]].*/)?([\\D-[/]][^/]*)$");

        static object matchSymbol(string s)
        {
            Match m = symbolPat.Match(s);
            if (m.Success)
            {
                int gc = m.Groups.Count;
                string ns = m.Groups[1].Value;
                string name = m.Groups[2].Value;
                if (ns != null && ns.EndsWith(":/")
                   || name.EndsWith(":")
                   || s.IndexOf("::", 1) != -1)
                    return null;
                // Maybe resolveSymbol should move here or into Namespace:  resolveSymbol is not used in the compiler.
                if (s.StartsWith("::"))
                {
                    Symbol ks = Symbol.intern(s.Substring(2));
                    Namespace kns;
                    if (ks.Namespace != null)
                        kns = Compiler.NamespaceFor(ks);
                    else
                        kns = Compiler.CurrentNamespace;
                    //auto-resolving keyword
                    return Keyword.intern(kns.Name.Name, ks.Name);
                }
                bool isKeyword = s[0] == ':';
                Symbol sym = Symbol.intern(s.Substring(isKeyword ? 1 : 0));
                if (isKeyword)
                    return Keyword.intern(sym);
                return sym;
            }
            return null;
        }



        #endregion

        #region Reading numbers

        static Regex intRE   = new Regex("^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)\\.?$");
        static Regex ratioRE = new Regex("^([-+]?[0-9]+)/([0-9]+)$");
        static Regex floatRE = new Regex("^[-+]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?[M]?$");


        static object readNumber(PushbackTextReader r, char initch)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append(initch);

            for (; ; )
            {
                int ch = r.Read();
                if (ch == -1 || isWhitespace(ch) || isMacro(ch))
                {
                    Unread(r, ch);
                    break;
                }
                sb.Append((char)ch);
            }

            string s = sb.ToString();
            object n = matchNumber(s);
            if (n == null)
                throw new FormatException("Invalid number: " + s);
            return n;
        }

        public static object matchNumber(string s)
        {
            Match m = intRE.Match(s);
            if ( m.Success )
            {
                if ( m.Groups[2].Success )
                    // matched 0 only
                    return 0;
                bool isNeg = m.Groups[1].Value == "-";
                string n = null;
                int radix = 10;
                if (m.Groups[3].Success)
                {
                    n = m.Groups[3].Value;
                    radix = 10;
                }
                else if (m.Groups[4].Success)
                {
                    n = m.Groups[4].Value;
                    radix = 16;
                }
                else if (m.Groups[5].Success)
                {
                    n = m.Groups[5].Value;
                    radix = 8;
                }
                else if (m.Groups[7].Success)
                {
                    n = m.Groups[7].Value;
                    radix = Int32.Parse(m.Groups[6].Value);
                }
                if (n == null)
                    return null;

                BigInteger bn = new BigInteger(n, radix);
                return Numbers.reduce(isNeg ? bn.negate() : bn);
            }
            m = floatRE.Match(s);

            if (m.Success)
            {
                return ( s[s.Length-1] == 'M' )
                    ? new BigDecimal( s.Substring(0,s.Length-1))  // TODO: Fix MS inadequacy
                    : (object)Double.Parse(s);
            }
            m = ratioRE.Match(s);
            if (m.Success)
            {
                // There is a bug in the BigInteger c-tor that causes it barf on a leading +.
                string numerString = m.Groups[1].Value;
                string denomString = m.Groups[2].Value;
                if (numerString[0] == '+')
                    numerString = numerString.Substring(1);
                return Numbers.BIDivide(new BigInteger(numerString), new BigInteger(denomString));
            }
            return null;
        }

        #endregion


        // Reader classes made public according to Java rev 1121

        public abstract class ReaderBase : AFn
        {
           public override object invoke(object arg1, object arg2)
            {
                return Read((PushbackTextReader)arg1, (Char)arg2);
            }

           protected abstract object Read(PushbackTextReader r, char c);
        }

        public sealed class CharacterReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char backslash)
            {
                int ch = r.Read();
                if (ch == -1)
                    throw new EndOfStreamException("EOF while reading character");
                String token = readToken(r, (char)ch);
                if (token.Length == 1)
                    return token[0];
                else if (token.Equals("newline"))
                    return '\n';
                else if (token.Equals("space"))
                    return ' ';
                else if (token.Equals("tab"))
                    return '\t';
                else if (token.Equals("backspace"))
                    return '\b';
                else if (token.Equals("formfeed"))
                    return '\f';
                else if (token.Equals("return"))
                    return '\r';
                else if (token.StartsWith("u"))
                {
                    char c = (char)readUnicodeChar(token, 1, 4, 16);
                    if (c >= '\uD800' && c <= '\uDFFF') // surrogate code unit?
                        throw new Exception("Invalid character constant: \\u" + ((int)c).ToString("X"));
                    return c;
                }
                else if (token.StartsWith("o"))
                {
                    int len = token.Length - 1;
                    if (len > 3)
                        throw new Exception("Invalid octal escape sequence length: " + len);
                    int uc = readUnicodeChar(token, 1, len, 8);
                    if (uc > 255) //octal377
                        throw new Exception("Octal escape sequence must be in range [0, 377].");
                    return (char)uc;
                }
                throw new Exception("Unsupported character: \\" + token);
            }
        }

        public sealed class StringReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char doublequote)
            {
                StringBuilder sb = new StringBuilder();

                for (int ch = r.Read(); ch != '"'; ch = r.Read())
                {
                    if (ch == -1)
                        throw new EndOfStreamException("EOF while reading string");
                    if (ch == '\\')	//escape
                    {
                        ch = r.Read();
                        if (ch == -1)
                            throw new EndOfStreamException("EOF while reading string");
                        switch (ch)
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
                                ch = r.Read();
                                if (CharValueInRadix(ch, 16) == -1)
                                    throw new Exception("Invalid unicode escape: \\u" + (char)ch);
                                ch = readUnicodeChar((PushbackTextReader)r, ch, 16, 4, true);
                                break;
                            default:
                                {
                                    if (CharValueInRadix(ch, 8) != -1)
                                    {
                                        ch = readUnicodeChar((PushbackTextReader)r, ch, 8, 3, false);
                                        if (ch > 255) //octal377
                                            throw new Exception("Octal escape sequence must be in range [0, 377].");
                                    }
                                    else
                                        throw new Exception("Unsupported escape character: \\" + (char)ch);
                                }
                                break;
                        }
                    }
                    sb.Append((char)ch);
                }
                return sb.ToString();
            }
        }

        public sealed class CommentReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char semicolon)
            {
                int ch;
                do
                {
                    ch = r.Read();
                } while (ch != -1 && ch != '\n' && ch != '\r');
                return r;
            }
        }

        public sealed class ListReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char leftparen)
            {
                int line = -1;
                if (r is LineNumberingTextReader)
                    line = ((LineNumberingTextReader)r).LineNumber;
                IList<Object> list = readDelimitedList(')', r, true);
                if (list.Count == 0)
                    return PersistentList.EMPTY;
                IObj s = (IObj)PersistentList.create((IList)list);
                //		IObj s = (IObj) RT.seq(list);
                if (line != -1)
                    return s.withMeta(RT.map(RT.LINE_KEY, line));
                else
                    return s;
            }
        }

        public sealed class VectorReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char leftparen)
            {
                return LazilyPersistentVector.create(readDelimitedList(']', r, true));
            }
        }

        public sealed class MapReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char leftbrace)
            {
                //return PersistentHashMap.create(readDelimitedList('}', r, true));
                return RT.map(readDelimitedList('}', r, true).ToArray());
            }
        }

        public sealed class UnmatchedDelimiterReader : ReaderBase
        {
            protected override object Read(PushbackTextReader reader, char rightdelim)
            {
                throw new Exception("Unmatched delimiter: " + rightdelim);
            }
        }

        public sealed class DiscardReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char underscore)
            {
                ReadAux(r);
                return r;
            }
        }

        public sealed class WrappingReader : ReaderBase
        {
            readonly Symbol _sym;

            public WrappingReader(Symbol sym)
            {
                _sym = sym;
            }

            protected override object Read(PushbackTextReader r, char quote)
            {
                //object o = read(r, true, null, true);
                object o = ReadAux(r);
                return RT.list(_sym, o);
            }
        }

        public sealed class SyntaxQuoteReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char backquote)
            {
                try
                {
                    Var.pushThreadBindings(RT.map(GENSYM_ENV, PersistentHashMap.EMPTY));
                    //object form = read(r, true, null, true);
                    object form = ReadAux(r);
                    return syntaxQuote(form);
                }
                finally
                {
                    Var.popThreadBindings();
                }
            }

            static object syntaxQuote(object form)
            {
                object ret;
                if (Compiler.isSpecial(form))
                    ret = RT.list(Compiler.QUOTE, form);
                else if (form is Symbol)
                {
                    Symbol sym = (Symbol)form;
                    if (sym.Namespace == null && sym.Name.EndsWith("#"))
                    {
                        IPersistentMap gmap = (IPersistentMap)GENSYM_ENV.deref();
                        if (gmap == null)
                            throw new InvalidDataException("Gensym literal not in syntax-quote");
                        Symbol gs = (Symbol)gmap.valAt(sym);
                        if (gs == null)
                            GENSYM_ENV.set(gmap.assoc(sym, gs = Symbol.intern(null,
                                                                              sym.Name.Substring(0, sym.Name.Length - 1)
                                                                              + "__" + RT.nextID() + "__auto__")));
                        sym = gs;
                    }
                    else if (sym.Namespace == null && sym.Name.EndsWith("."))
                    {
                        Symbol csym = Symbol.intern(null, sym.Name.Substring(0, sym.Name.Length - 1));
                        csym = Compiler.resolveSymbol(csym);
                        sym = Symbol.intern(null, csym.Name + ".");
                    }
                    else if ( sym.Namespace == null && sym.Name.StartsWith("."))
                    {
                        // simply quote method names
                    }
                    else
                        sym = Compiler.resolveSymbol(sym);
                    ret = RT.list(Compiler.QUOTE, sym);
                }
                //else if (form is Unquote)  
                //    return ((Unquote)form).Obj;
                // Rev 1184
                else if (isUnquote(form))
                    return RT.second(form);
                else if (isUnquoteSplicing(form))
                    throw new ArgumentException("splice not in list");
                else if (form is IPersistentCollection)
                {
                    if (form is IPersistentMap)
                    {
                        IPersistentVector keyvals = flattenMap(form);
                        ret = RT.list(APPLY, HASHMAP, RT.list(SEQ,RT.cons(CONCAT, sqExpandList(keyvals.seq()))));
                    }
                    else if (form is IPersistentVector)
                    {
                        ret = RT.list(APPLY, VECTOR, RT.list(SEQ,RT.cons(CONCAT, sqExpandList(((IPersistentVector)form).seq()))));
                    }
                    else if (form is IPersistentSet)
                    {
                        ret = RT.list(APPLY, HASHSET,  RT.list(SEQ,RT.cons(CONCAT, sqExpandList(((IPersistentSet)form).seq()))));
                    }
                    else if (form is ISeq || form is IPersistentList)
                    {
                        ISeq seq = RT.seq(form);
                        if (seq == null)
                            ret = RT.cons(LIST, null);
                        else
                            ret =  RT.list(SEQ,RT.cons(CONCAT, sqExpandList(seq)));
                    }
                    else
                        throw new InvalidOperationException("Unknown Collection type");
                }
                else if (form is Keyword
                        || Util.IsNumeric(form)
                        || form is Char
                        || form is String)
                    ret = form;
                else
                    ret = RT.list(Compiler.QUOTE, form);

                if (form is IObj &&  RT.meta(form) != null)
                {
                    //filter line numbers
                    IPersistentMap newMeta = ((IObj)form).meta().without(RT.LINE_KEY);
                    if (newMeta.count() > 0)
                        return RT.list(WITH_META, ret, syntaxQuote(((IObj)form).meta()));
                }
                return ret;
            }

            private static ISeq sqExpandList(ISeq seq)
            {
                IPersistentVector ret = PersistentVector.EMPTY;
                for (; seq != null; seq = seq.next())
                {
                    Object item = seq.first();
                    //if (item is Unquote)
                    //    ret = ret.cons(RT.list(LIST, ((Unquote)item).Obj));
                    // REV 1184
                    if (isUnquote(item))
                        ret = ret.cons(RT.list(LIST, RT.second(item)));
                    else if (isUnquoteSplicing(item))
                        ret = ret.cons(RT.second(item));
                    else
                        ret = ret.cons(RT.list(LIST, syntaxQuote(item)));
                }
                return ret.seq();
            }

            private static IPersistentVector flattenMap(object form)
            {
                IPersistentVector keyvals = PersistentVector.EMPTY;
                for (ISeq s = RT.seq(form); s != null; s = s.next())
                {
                    IMapEntry e = (IMapEntry)s.first();
                    keyvals = (IPersistentVector)keyvals.cons(e.key());
                    keyvals = (IPersistentVector)keyvals.cons(e.val());
                }
                return keyvals;
            }
        }

        sealed class UnquoteReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char comma)
            {
                int ch = r.Read();
                if (ch == -1)
                    throw new EndOfStreamException("EOF while reading character");
                if (ch == '@')
                {
                    //object o = read(r, true, null, true);
                    object o = ReadAux(r);
                    return RT.list(UNQUOTE_SPLICING, o);
                }
                else
                {
                    Unread(r, ch);
                    //object o = read(r, true, null, true);
                    object o = ReadAux(r);
                    //return new Unquote(o);
                    // per Rev 1184
                    return RT.list(UNQUOTE, o);
                }
            }
        }

        #region Unquote helpers

        // Per rev 1184
        static bool isUnquote(object form)
        {
            return form is ISeq && Util.equals(RT.first(form),UNQUOTE);
        }

        static bool isUnquoteSplicing(object form)
        {
            return form is ISeq && Util.equals(RT.first(form), UNQUOTE_SPLICING);
        }

        #endregion

        public sealed class DispatchReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char hash)
            {
                int ch = r.Read();
                if (ch == -1)
                    throw new EndOfStreamException("EOF while reading character");
                IFn fn = _dispatchMacros[ch];
                if (fn == null)
                    throw new Exception(String.Format("No dispatch macro for: {0}", (char)ch));
                return fn.invoke(r, (char)ch);
            }
        }

        public sealed class MetaReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char caret)
            {
                int line = -1;
                if (r is LineNumberingTextReader)
                    line = ((LineNumberingTextReader)r).LineNumber;
                //object meta = read(r, true, null, true);
                object meta = ReadAux(r);
                if (meta is Symbol || meta is Keyword || meta is String)
                    meta = RT.map(RT.TAG_KEY, meta);
                else if (!(meta is IPersistentMap))
                    throw new ArgumentException("Metadata must be Symbol,Keyword,String or Map");

                //object o = read(r, true, null, true);
                object o = ReadAux(r);
                if (o is IMeta)
                {
                    if (line != -1 && o is ISeq)
                        meta = ((IPersistentMap)meta).assoc(RT.LINE_KEY, line);
                    if (o is IReference)
                    {
                        ((IReference)o).resetMeta((IPersistentMap)meta);
                        return o;
                    }
                    return ((IObj)o).withMeta((IPersistentMap)meta);
                }
                else
                    throw new ArgumentException("Metadata can only be applied to IMetas");
            }
        }

        public sealed class VarReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char quote)
            {
                //object o = read(r, true, null, true);
                object o = ReadAux(r);
                //		if(o instanceof Symbol)
                //			{
                //			Object v = Compiler.maybeResolveIn(Compiler.currentNS(), (Symbol) o);
                //			if(v instanceof Var)
                //				return v;
                //			}
                return RT.list(THE_VAR, o);
            }
        }

        public sealed class RegexReader : ReaderBase
        {
            static readonly StringReader stringrdr = new StringReader();

            protected override object Read(PushbackTextReader r, char doublequote)
            {
                StringBuilder sb = new StringBuilder();
                for (int ch = r.Read(); ch != '"'; ch = r.Read())
                {
                    if (ch == -1)
                        throw new EndOfStreamException("EOF while reading regex");
                    sb.Append((char)ch);
                    if (ch == '\\')	//escape
                    {
                        ch = r.Read();
                        if (ch == -1)
                            throw new EndOfStreamException("EOF while reading regex");
                        sb.Append((char)ch);
                    }
                }
                return new Regex(sb.ToString());
            }
        }

        public sealed class FnReader : ReaderBase
        {
            //static ListReader _listReader = new ListReader();

            protected override object Read(PushbackTextReader r, char lparen)
            {
                if (ARG_ENV.deref() != null)
                    throw new InvalidOperationException("Nested #()s are not allowed");
                try
                {
                    Var.pushThreadBindings(RT.map(ARG_ENV, PersistentTreeMap.EMPTY));
                    r.Unread('(');
                    ////object form = ReadAux(r, true, null, true);
                    object form = ReadAux(r);
                    //object form = _listReader.invoke(r, '(');

                    IPersistentVector args = PersistentVector.EMPTY;
                    PersistentTreeMap argsyms = (PersistentTreeMap)ARG_ENV.deref();
                    ISeq rargs = argsyms.rseq();
                    if (rargs != null)
                    {
                        int higharg = (int)((IMapEntry)rargs.first()).key();
                        if (higharg > 0)
                        {
                            for (int i = 1; i <= higharg; ++i)
                            {
                                object sym = argsyms.valAt(i);
                                if (sym == null)
                                    sym = garg(i);
                                args = args.cons(sym);
                            }
                        }
                        object restsym = argsyms.valAt(-1);
                        if (restsym != null)
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

        sealed class ArgReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char pct)
            {
                int ch = r.Read();
                Unread(r, ch);
                //% alone is first arg
                if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch))
                {
                    return registerArg(1);
                }
                //object n = ReadAux(r, true, null, true);
                object n = ReadAux(r);
                if (n.Equals(Compiler._AMP_))
                    return registerArg(-1);
                if (!Util.IsNumeric(n))
                    throw new ArgumentException("arg literal must be %, %& or %integer");
                return registerArg(Util.ConvertToInt(n));
            }

            static Symbol registerArg(int n)
            {
                PersistentTreeMap argsyms = (PersistentTreeMap)ARG_ENV.deref();
                if (argsyms == null)
                {
                    throw new InvalidOperationException("arg literal not in #()");
                }
                Symbol ret = (Symbol)argsyms.valAt(n);
                if (ret == null)
                {
                    ret = garg(n);
                    ARG_ENV.set(argsyms.assoc(n, ret));
                }
                return ret;
            }
        }

        public sealed class SetReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char leftbracket)
            {
                return PersistentHashSet.create1(readDelimitedList('}', r, true));
            }
        }

        //TODO: Need to figure out who to deal with typenames in the context of multiple loaded assemblies.
        public sealed class EvalReader : ReaderBase
        {
            protected override object Read(PushbackTextReader r, char eq)
            {
                if (!RT.booleanCast(RT.READEVAL.deref()))
                {
                    throw new Exception("EvalReader not allowed when *read-eval* is false.");
                }
                Object o = read(r, true, null, true);
                if (o is Symbol)
                {
                    return RT.classForName(o.ToString());
                }
                else if (o is IPersistentList)
                {
                    Symbol fs = (Symbol)RT.first(o);
                    if (fs.Equals(THE_VAR))
                    {
                        Symbol vs = (Symbol)RT.second(o);
                        return RT.var(vs.Namespace, vs.Name);  //Compiler.resolve((Symbol) RT.second(o),true);
                    }
                    if (fs.Name.EndsWith("."))
                    {
                        Object[] args = RT.toArray(RT.next(o));
                        return Reflector.InvokeConstructor(RT.classForName(fs.Name.Substring(0, fs.Name.Length - 1)), args);
                    }
                    if (Compiler.NamesStaticMember(fs))
                    {
                        Object[] args = RT.toArray(RT.next(o));
                        return Reflector.InvokeStaticMethod(fs.Namespace, fs.Name, args);
                    }
                    Object v = Compiler.maybeResolveIn(Compiler.CurrentNamespace, fs);
                    if (v is Var)
                    {
                        return ((IFn)v).applyTo(RT.next(o));
                    }
                    throw new Exception("Can't resolve " + fs);
                }
                else
                    throw new ArgumentException("Unsupported #= form");
            }
        }

        public sealed class UnreadableReader : ReaderBase
        {
            protected override object Read(PushbackTextReader reader, char leftangle)
            {
                throw new Exception("Unreadable form");
            }
        }

        public sealed class ReaderException : Exception
        {
            readonly int _line;

            public int Line
            {
                get { return _line; }
            } 

            public ReaderException(int line, Exception e)
                : base(null, e)
            {
                _line = line;
            }
        }
    }
}
