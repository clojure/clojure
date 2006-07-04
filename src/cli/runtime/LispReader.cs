/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.IO;
using System.Text.RegularExpressions;
using java.math;
using System.Text;
using System.Collections;

namespace clojure.lang
{


public class LispReader {

static IFn[] macros = new IFn[256];
static Regex symbolPat = new Regex("[:]?[\\D-[:\\.]][^:\\.]*",RegexOptions.Compiled);
	static Regex varPat = new Regex("([\\D-[:\\.]][^:\\.]*):([\\D-[:\\.]][^:\\.]*)", RegexOptions.Compiled);
	static Regex intPat = new Regex("[-+]?[0-9]+\\.?", RegexOptions.Compiled);
	static Regex ratioPat = new Regex("([-+]?[0-9]+)/([0-9]+)", RegexOptions.Compiled);
	static Regex floatPat = new Regex("[-+]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?", RegexOptions.Compiled);

	static Regex accessorPat = new Regex("\\.[a-zA-Z_]\\w*", RegexOptions.Compiled);
	static Regex instanceMemberPat = new Regex("\\.([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)", RegexOptions.Compiled);
	static Regex staticMemberPat = new Regex("([a-zA-Z_][\\w\\.]*)\\.([a-zA-Z_]\\w*)", RegexOptions.Compiled);
	static Regex classNamePat = new Regex("([a-zA-Z_][\\w\\.]*)\\.", RegexOptions.Compiled);

static LispReader(){
macros['"'] = new StringReader();
macros[';'] = new CommentReader();
macros['('] = new ListReader();
macros[')'] = new UnmatchedDelimiterReader();
macros['\\'] = new CharacterReader();
}

static public Object read(LineNumberingTextReader r, bool eofIsError, Object eofValue, bool isRecursive)
        {

    for (; ;)
        {
        int ch = r.Read();

        while (Char.IsWhiteSpace((char)ch))
            ch = r.Read();

        if (ch == -1)
            {
            if (eofIsError)
                throw new Exception("EOF while reading");
            return eofValue;
            }

        IFn macroFn = getMacro(ch);
        if (macroFn != null)
            {
            Object ret = macroFn.invoke(r, (char)ch);
            if(RT.suppressRead())
                return null;
            //no op macros return the reader
            if (ret == r)
                continue;
            return ret;
            }

        String token = readToken(r,(char)ch);
        if(RT.suppressRead())
            return null;
        return interpretToken(token);
        }
}

static private String readToken(LineNumberingTextReader r, char initch) {
    StringBuilder sb = new StringBuilder();
    sb.Append(initch);

    for(;;)
        {
        int ch = r.Read();
        if(ch == -1 || Char.IsWhiteSpace((char)ch) || isMacro(ch))
            {
            r.unread(ch);
            return sb.ToString();
            }
		sb.Append((char)ch);
        }
}

static private Object interpretToken(String s) {
    if (s.Equals("null"))
        {
        return null;
        }
    Object ret = null;
    ret = matchSymbol(s);
    if(ret != null)
        return ret;
    ret = matchVar(s);
    if(ret != null)
        return ret;
    ret = matchNumber(s);
    if(ret != null)
        return ret;
    ret = matchHostName(s);
    if(ret != null)
        return ret;


    throw new InvalidDataException("Invalid syntax: " + s);
}

private static Object matchHostName(String s) {
    Match m = accessorPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return new Accessor(s);
    m = classNamePat.Match(s);
    if(m.Success && m.Length == s.Length)
        return new ClassName(RT.resolveClassNameInContext(m.Groups[1].Value));
    m = instanceMemberPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return new InstanceMemberName(RT.resolveClassNameInContext(m.Groups[1].Value),m.Groups[2].Value);
    m = staticMemberPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return new StaticMemberName(RT.resolveClassNameInContext(m.Groups[1].Value),m.Groups[2].Value);

    return null;
}

private static Object matchSymbol(String s) {
    Match m = symbolPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return Symbol.intern(s);
    return null;
}

private static Object matchVar(String s) {
    Match m = varPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return Namespace.intern(m.Groups[1].Value,m.Groups[2].Value);
    return null;
}

private static Object matchNumber(String s) {
    Match m = intPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return Num.from(new BigInteger(s));
    m = floatPat.Match(s);
    if(m.Success && m.Length == s.Length)
        return Num.from(Double.Parse(s));
    m = ratioPat.Match(s);
    if(m.Success && m.Length == s.Length)
        {
        return Num.divide(new BigInteger(m.Groups[1].Value),new BigInteger(m.Groups[2].Value));
        }
    return null;
}

static private IFn getMacro(int ch) {
    if (ch < macros.Length)
        return macros[ch];
    return null;
}

static private bool isMacro(int ch) {
    return (ch < macros.Length && macros[ch] != null);
}


class StringReader : AFn{
    override public Object invoke(Object reader, Object doublequote) {
        StringBuilder sb = new StringBuilder();
        LineNumberingTextReader r = (LineNumberingTextReader) reader;

		for(int ch = r.Read();ch != '"';ch = r.Read())
			{
			if(ch == -1)
                throw new Exception("EOF while reading string");
			if(ch == '\\')	//escape
				{
				ch = r.Read();
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
					default:
						throw new Exception("Unsupported escape character: \\" + (char)ch);
					}
				}
			sb.Append((char)ch);
			}
        return sb.ToString();
    }

}
class CommentReader : AFn{
	override public Object invoke(Object reader, Object semicolon)
		{
        LineNumberingTextReader r = (LineNumberingTextReader) reader;
        int ch;
        do
            {
            ch = r.Read();
            } while (ch != -1 && ch != '\n' && ch != '\r');
        return r;
    }

}
class CharacterReader : AFn{
	override public Object invoke(Object reader, Object backslash)
		{
        LineNumberingTextReader r = (LineNumberingTextReader) reader;
        int ch = r.Read();
        if(ch == -1)
            throw new Exception("EOF while reading character");
        String token = readToken(r,(char)ch);
        if(token.Length == 1)
            return token[0];
        else if(token.Equals("newline"))
            return '\n';
        else if(token.Equals("space"))
            return ' ';
        else if(token.Equals("tab"))
            return '\t';
        throw new Exception("Unsupported character: \\" + token);
    }

}
class ListReader : AFn{
    override public Object invoke(Object reader, Object leftparen) {
        LineNumberingTextReader r = (LineNumberingTextReader) reader;
        return readDelimitedList(')', r, true);
    }

}
class UnmatchedDelimiterReader : AFn{
    override public Object invoke(Object reader, Object rightdelim) {
        throw new Exception("Unmatched delimiter: " + rightdelim);
    }

}
public static ISeq readDelimitedList(char delim, LineNumberingTextReader r, bool isRecursive) {
    ArrayList a = new ArrayList();

    for (; ;)
        {
        int ch = r.Read();

        while (Char.IsWhiteSpace((char)ch))
			ch = r.Read();

        if (ch == -1)
			throw new Exception("EOF while reading");

        if(ch == delim)
			break;

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
			r.unread(ch);
			
			Object o = read(r, true, null, isRecursive);
            if (o != r)
                a.Add(o);
            }
        }

    ISeq ret = null;
    for(int i=a.Count-1;i>=0;--i)
        ret = RT.cons(a[i], ret);
    return ret;
}

/*
public static void Main(String[] args){
    LineNumberingTextReader r = new LineNumberingTextReader(Console.In);
    TextWriter w = Console.Out;
    Object ret = null;
    try{
        for(;;)
            {
            ret = LispReader.read(r, true, null, false);
            RT.print(ret, w);
            w.Write('\n');
            w.Flush();
            }
        }
    catch(Exception e)
        {
        //e.printStackTrace();
        Console.Error.WriteLine(e.StackTrace);
        }
}
//*/

}

}

