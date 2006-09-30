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
using System.Threading;

namespace clojure.lang
{

public class RT
{

    public static Symbol T = Symbol.intern("t");
	static public Var OUT = Module.intern("clojure", "^out");
	public static Object[] EMPTY_ARRAY = new Object[0];


    static public readonly Object[] chars;

	static int id = 1;
	static public int nextID()
		{
		return Interlocked.Increment(ref id);
		}	
	
	static RT(){
		chars = new Object[256];
        for(int i=0;i<chars.Length;i++)
			chars[i] = (char)i;
        }

static public bool equal(Object k1,Object k2){
    return k1 == k2 ||
           (k1 != null && k1.Equals(k2));
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

static public int hash(Object o){
    if(o == null)
        return 0;
    return o.GetHashCode();
}

static public int hashCombine(int seed, int hash){
    //a la boost
    seed ^= (int)(hash + 0x9e3779b9 + (seed << 6) + (seed >> 2));
    return seed;
}

static public ISeq seq(Object coll)  {
    if(coll == null)
        return null;
    else if(coll is IPersistentCollection)
        return ((IPersistentCollection) coll).seq();
	else if (coll is IEnumerable)
		return EnumeratorSeq.create(((IEnumerable)coll).GetEnumerator());
    else if(coll is Object[])
        return ArraySeq.create((Object[]) coll);
    else
		throw new ArgumentException("Don't know how to create ISeq from arg");
}

static public Object meta(Object x)
	{
	if (x == null)
		return null;
	return ((Obj)x).meta();
	}

public static int count(Object o)
	{
	if (o == null)
		return 0;
	return ((IPersistentCollection)o).count();
	}

static public IPersistentCollection cons(Object x, IPersistentCollection y)
	{
	if (y == null)
		return new PersistentList(x);
	return y.cons(x);
	}

static public ISeq cons(Object x, ISeq y)
	{
	if (y == null)
		return new PersistentList(x);
	return (ISeq)y.cons(x);
	}

static public Object first(Object x)
	{
	if (x == null)
		return null;
	return seq(x).first();
	}
	
static public Object second(Object x)
	{
	return first(rest(x));
	}

static public Object third(Object x)
	{
	return first(rest(rest(x)));
	}
	
static public ISeq rest(Object x)
	{
	if (x == null)
		return null;
	return seq(x).rest();
	}

static public Object peek(Object x)
	{
	if (x == null)
		return null;
	return ((IPersistentList)x).peek();
	}

static public Object pop(Object x)
	{
	if (x == null)
		return null;
	return ((IPersistentList)x).pop();
	}

static public Object get(Object key, Object coll)
	{
	if (coll == null)
		return null;
	return ((Associative)coll).get(key);
	}

static public Object assoc(Object key, Object val, Object coll)
	{
	if (coll == null)
		return new MapEntry(key, val);
	return ((Associative)coll).assoc(key, val);
	}

static public Object contains(Object key, Object coll)
	{
	if (coll == null)
		return false;
	return ((Associative)coll).contains(key);
	}

static public Object find(Object key, Object coll)
	{
	if (coll == null)
		return null;
	return ((Associative)coll).find(key);
	}

	//takes a seq of key,val,key,val
//returns tail starting at val of matching key if found, else null
static public ISeq findKey(Keyword key,ISeq keyvals)  {
    while(keyvals != null)
        {
        ISeq r = keyvals.rest();
        if (r == null)
            throw new Exception("Malformed keyword argslist");
        if (keyvals.first() == key)
            return r;
        keyvals = r.rest();
        }
    return null;
}

static public Object without(Object key, Object coll)
	{
	if (coll == null)
		return null;
	return ((IPersistentMap)coll).without(key);
	}

static public Object nth(int n, Object coll) {
    if(coll == null)
        return null;
    else if(coll is IPersistentArray)
        return ((IPersistentArray)coll).nth(n);
    else if(coll is Object[])
        return ((Object[])coll)[n];
    else if(coll is Sequential)
        {
        ISeq seq = ((IPersistentCollection) coll).seq();
        for(int i=0;i<=n && seq != null;++i, seq = seq.rest())
            {
            if(i == n)
                return seq.first();
            }
        return null;
        }
    else
        return null;
}

static public Object assocN(int n, Object val, Object coll) {
    if(coll == null)
        return null;
    else if(coll is IPersistentArray)
        return ((IPersistentArray)coll).assocN(n,val);
    else if(coll is Object[])
        {
        //hmm... this is not persistent
        Object[] array = ((Object[])coll);
        array[n] = val;
        return array;
        }
    else
        return null;
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

static public ISeq list()
	{
	return null;
	}

static public ISeq list(Object arg1)
	{
	return new PersistentList(arg1);
	}

static public ISeq list(Object arg1, Object arg2)
	{
	return listStar(arg1, arg2, null);
	}

static public ISeq list(Object arg1, Object arg2, Object arg3)
	{
	return listStar(arg1, arg2, arg3, null);
	}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4)
	{
	return listStar(arg1, arg2, arg3, arg4, null);
	}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
	{
	return listStar(arg1, arg2, arg3, arg4, arg5, null);
	}

static public ISeq listStar(Object arg1, ISeq rest)
	{
	return cons(arg1, rest);
	}

static public ISeq listStar(Object arg1, Object arg2, ISeq rest)
	{
	return cons(arg1, cons(arg2, rest));
	}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, rest)));
	}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
	}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
	}

static public ISeq arrayToList(Object[] a)
    {
        ISeq ret = null;
        for (int i = a.Length - 1; i >= 0; --i)
            ret = cons(a[i], ret);
        return ret;
    }

static public Object[] seqToArray(ISeq seq) {
    int len = length(seq);
    Object[] ret = new Object[len];
    for(int i=0;seq != null;++i, seq = seq.rest())
        ret[i] = seq.first();
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

static public void formatAesthetic(TextWriter w, Object obj) {
    if(obj == null)
        w.Write("null");
    else
        w.Write(obj.ToString());
}

static public void formatStandard(TextWriter w,Object obj) {
    if(obj == null)
		w.Write("null");
    else if(obj is String)
        {
		w.Write('"');
		w.Write((String)obj);
		w.Write('"');
        }
    else if(obj is Char)
        {
		w.Write('\\');
		char c = (Char)obj;
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
    else
		w.Write(obj.ToString());
}

static public Object format(Object o, String s, params Object[] args) {
	TextWriter w;
	if(o == null)
		w = new StringWriter();
	else if(equal(o,T))
		w = (TextWriter)OUT.getValue();
	else
		w = (TextWriter)o;
	doFormat(w,s,ArraySeq.create(args));
	if(o == null)
		return w.ToString();
	return null;
}

static public void doFormat(TextWriter w, String s, ISeq args) {
    for (int i = 0; i < s.Length;)
        {
        char c = s[i++];
        switch (Char.ToLower(c))
            {
            case '~':
                char d = s[i++];
                switch (Char.ToLower(d))
                    {
                    case '%':
                        w.Write('\n');
                        break;
                    case 't':
                        w.Write('\t');
                        break;
                    case 'a':
                        if(args == null)
                            throw new Exception("Missing argument");
                        RT.formatAesthetic(w, RT.first(args));
                        args = RT.rest(args);
                        break;
                    case 's':
                        if(args == null)
							throw new Exception("Missing argument");
                        RT.formatStandard(w, RT.first(args));
                        args = RT.rest(args);
                        break;
                    case '{':
                        int j = s.IndexOf("~}", i);    //note - does not nest
                        if(j == -1)
							throw new Exception("Missing ~}");
                        String subs = s.Substring(i, j-i);
                        format(w, subs, RT.seq(RT.first(args)));
                        args = RT.rest(args);
                        i = j+2; //skip ~}
                        break;
                    case '^':
                        if(args == null)
                            return;
                        break;
                    case '~':
                        w.Write('~');
                        break;
                    default:
						throw new Exception("Unsupported ~ directive: " + d);
                    	break;
                    }
                break;
            default:
                w.Write(c);
            	break;
            }
        }
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