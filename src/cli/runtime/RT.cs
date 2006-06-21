/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:28:27 PM */

using System;
using System.Collections;
using System.IO;

namespace clojure.lang
{

public class RT
{

    public static Symbol T = Symbol.intern("t");
    public static Object[] EMPTY_ARRAY = new Object[0];


    static public readonly Object[] chars;

	static RT(){
		chars = new Object[256];
        for(int i=0;i<chars.Length;i++)
			chars[i] = (char)i;
        }

    static public Object eq(Object arg1, Object arg2) {
        return (arg1 == arg2)?T:null;
        }

    static public Object eql(Object arg1, Object arg2) {
        if(arg1 == arg2)
        	return T;
        if(arg1 == null || arg2 == null)
        	return null;
        if(arg1 is Num
        		&& arg1.GetType() == arg2.GetType()
				&& arg1.Equals(arg2))
        	return T;
        if(arg1 is Char
        		&& arg2 is Char
				&& arg1.Equals(arg2))
        	return T;
        return null;
        }

	//static public Object equal(Object arg1, Object arg2) {
	//    if(arg1 == null)
	//        return arg2 == null ? T : null;
	//    else if(arg2 == null)
	//        return null;
	//    return (eql(arg1,arg2) != null
	//            || (arg1 is Cons
	//                    && arg2 is Cons
	//                    && equal(((Cons)arg1).first(),((Cons)arg2).first())!=null
	//                    && equal(((Cons)arg1).rest(),((Cons)arg2).rest())!=null))
	//    ?T:null;
	//    }

static public ISeq seq(Object coll) {
    if(coll == null || coll is ISeq)
        return (ISeq) coll;
    else if(coll is ISequential)
        return ((ISequential) coll).seq();
    else if(coll is Object[])
        return ArraySeq.create((Object[]) coll);
    else
        throw new ArgumentException("Don't know how to create ISeq from arg");
}

    static public Iter iter(Object coll)
        {
        if (coll == null || coll is Iter)
            return (Iter)coll;
        else if (coll is IEnumerable)
            {
            IEnumerator e = ((IEnumerable)coll).GetEnumerator();
            if (e.MoveNext())
                return new EnumeratorIter(e);
            return null;
            }
        else
            {
            throw new ArgumentException("Don't know how to create Iter from arg");
            }

        }

    static public Object box(Object x)
        {
            return x;
        }

    static public Object box(char x)
        {
		if (x < chars.Length)
			return chars[x]; 
		return x;
        }

    static public Object box(bool x)
        {
            return x;
        }

    static public Object box(byte x)
        {
            return x;
        }

    static public Object box(short x)
        {
            return x;
        }

    static public Object box(int x)
        {
            return x;
        }

    static public Object box(long x)
        {
            return x;
        }

    static public Object box(float x)
        {
            return x;
        }

    static public Object box(double x)
        {
            return x;
        }

    static public char charCast(Object x)	{    return Convert.ToChar(x);	}

    static public bool booleanCast(Object x)	{	if(x is Boolean)
        return (bool)x; ;	return x != null;	}

    static public byte byteCast(Object x)
    {
        return Convert.ToByte(x);
    }

    static public short shortCast(Object x)
    {
    return Convert.ToInt16(x);    }

    static public int intCast(Object x)
    {
        return Convert.ToInt32(x);
    }

    static public long longCast(Object x)
    {
        return Convert.ToInt64(x);
    }

    static public float floatCast(Object x)
    {
        return Convert.ToSingle(x);
    }

    static public double doubleCast(Object x)
    {
        return Convert.ToDouble(x);
    }
static public Cons cons(Object x, Object y) {
return new Cons(x, seq(y));
}

static public Object first(Object x) {
    return seq(x).first();
}

static public ISeq rest(Object x) {
    return seq(x).rest();
}

static public Cons list()
	{
	return null;
	}

static public Cons list(Object arg1)
	{
	return cons(arg1, null);
	}

static public Cons list(Object arg1, Object arg2)
	{
	return listStar(arg1, arg2, null);
	}

static public Cons list(Object arg1, Object arg2, Object arg3)
	{
	return listStar(arg1, arg2, arg3, null);
	}

static public Cons list(Object arg1, Object arg2, Object arg3, Object arg4)
	{
	return listStar(arg1, arg2, arg3, arg4, null);
	}

static public Cons list(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
	{
	return listStar(arg1, arg2, arg3, arg4, arg5, null);
	}

static public Cons listStar(Object arg1, ISeq rest)
	{
	return cons(arg1, rest);
	}

static public Cons listStar(Object arg1, Object arg2, ISeq rest)
	{
	return cons(arg1, cons(arg2, rest));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, rest)));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
	}

static public Cons arrayToList(Object[] a)
    {
        Cons ret = null;
        for (int i = a.Length - 1; i >= 0; --i)
            ret = cons(a[i], ret);
        return ret;
    }

static public int length(ISeq list)
	{
	int i = 0;
	for(ISeq c = list; c != null; c = c.rest())
		{
		i++;
		}
	return i;
	}

static public int boundedLength(ISeq list, int limit)
	{
	int i = 0;
	for(ISeq c = list; c != null && i <= limit; c = c.rest())
		{
		i++;
		}
	return i;
	}


///////////////////////////////// reader support ////////////////////////////////

static Object readRet(int ret){
    if(ret == -1)
        return null;
    return box((char) ret);
}

static public Object readChar(TextReader r)  {
    int ret = r.Read();
    return readRet(ret);
}

static public Object peekChar(TextReader r)  {
    return readRet(r.Peek());
}

static public int getLineNumber(TextReader r)
    {
	if (r is LineNumberingTextReader)
		return ((LineNumberingTextReader)r).getLineNumber();
    return 0;
    }

static public TextReader getLineNumberingReader(TextReader r)
    {
	if (isLineNumberingReader(r))
        return r;
	return new LineNumberingTextReader(r);
    }

static public bool isLineNumberingReader(TextReader r)
    {
	return r is LineNumberingTextReader;
    }


static public String resolveClassNameInContext(String className) {
    //todo - look up in context var
    return className;
}

static public bool suppressRead(){
    //todo - look up in suppress-read var
    return false;
}

static public void print(Object x, TextWriter w) {
    //todo - make extensible
    if(x == null)
        w.Write("null");
    else if(x is ISeq)
        {
        w.Write('(');
        for(ISeq s = (ISeq)x;s != null;s = s.rest())
            {
            print(s.first(), w);
            if(s.rest()!=null)
                w.Write(' ');
            }
        w.Write(')');
        }
    else if(x is String)
        {
        w.Write('"');
        w.Write(x.ToString());
        w.Write('"');
        }
    else if(x is Char)
        {
        w.Write('\\');
        char c = (char)x;
        switch(c){
            case '\n':
                w.Write("newline");
                break;
            case '\t':
                w.Write("tab");
                break;
            case ' ':
                w.Write("space");
                break;
            default:
                w.Write(c);
                break;
            }
        }
    else w.Write(x.ToString());
}
/*-------------------------------- values --------------*/

static public Object setValues(params Object[] vals)
    {
    ThreadLocalData.setValues(vals);
    if(vals.Length > 0)
        return vals[0];
    return null;
    }

}
}