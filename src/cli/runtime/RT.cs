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

namespace org.clojure.runtime
{

public class RT
{

    public static Symbol T = Symbol.intern("t");

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

    static public Object equal(Object arg1, Object arg2) {
        if(arg1 == null)
        	return arg2 == null ? T : null;
        else if(arg2 == null)
            return null;
    	return (eql(arg1,arg2) != null
    			|| (arg1 is Cons
    					&& arg2 is Cons
						&& equal(((Cons)arg1).first,((Cons)arg2).first)!=null
						&& equal(((Cons)arg1).rest,((Cons)arg2).rest)!=null))
		?T:null;
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
static public Cons cons(Object x, Cons y)
	{
	return new Cons(x, y);
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

static public Cons listStar(Object arg1, Cons rest)
	{
	return cons(arg1, rest);
	}

static public Cons listStar(Object arg1, Object arg2, Cons rest)
	{
	return cons(arg1, cons(arg2, rest));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Cons rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, rest)));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, Cons rest)
	{
	return cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
	}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons rest)
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

static public int length(Cons list)
	{
	int i = 0;
	for(Cons c = list; c != null; c = c.rest)
		{
		i++;
		}
	return i;
	}

static public int boundedLength(Cons list, int limit)
	{
	int i = 0;
	for(Cons c = list; c != null && i <= limit; c = c.rest)
		{
		i++;
		}
	return i;
	}

static public Object setValues(ThreadLocalData tld, Object arg1)
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 1;
	tld.mvArray[0] = arg1;
	return arg1;
	}

static public Object setValues(ThreadLocalData tld, Object arg1, Object arg2)
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 2;
	tld.mvArray[0] = arg1;
	tld.mvArray[1] = arg2;
	return arg1;
	}

static public Object setValues(ThreadLocalData tld, Object arg1, Object arg2, Object arg3)
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 3;
	tld.mvArray[0] = arg1;
	tld.mvArray[1] = arg2;
	tld.mvArray[2] = arg3;
	return arg1;
	}

static public Object setValues(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4)
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 4;
	tld.mvArray[0] = arg1;
	tld.mvArray[1] = arg2;
	tld.mvArray[2] = arg3;
	tld.mvArray[3] = arg4;
	return arg1;
	}

static public Object setValues(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4,
                               Object arg5)
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 5;
	tld.mvArray[0] = arg1;
	tld.mvArray[1] = arg2;
	tld.mvArray[2] = arg3;
	tld.mvArray[3] = arg4;
	tld.mvArray[4] = arg5;
	return arg1;
	}

static public Object setValues(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4,
                               Object arg5, Cons args) /*throws Exception*/
	{
	if(tld == null)
		tld = ThreadLocalData.get();
	tld.mvCount = 5;
	tld.mvArray[0] = arg1;
	tld.mvArray[1] = arg2;
	tld.mvArray[2] = arg3;
	tld.mvArray[3] = arg4;
	tld.mvArray[4] = arg5;
	for(int i = 5; args != null && i < ThreadLocalData.MULTIPLE_VALUES_LIMIT; i++, args = args.rest)
		{
		tld.mvArray[i] = args.first;
		}
	if(args != null)
		throw new ArgumentException("Too many arguments to values (> ThreadLocalData.MULTIPLE_VALUES_LIMIT)");
	return arg1;
	}

}
}