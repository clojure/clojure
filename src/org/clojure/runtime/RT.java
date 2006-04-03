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

package org.clojure.runtime;

import java.util.Iterator;

public class RT{

    static public Object eq(Object arg1, Object arg2) {
        return (arg1 == arg2)?Boolean.TRUE:null;
        }

    static public Object eql(Object arg1, Object arg2) {
        if(arg1 == arg2)
	        return Boolean.TRUE;
        if(arg1 == null || arg2 == null)
	        return null;
        if(arg1 instanceof Num
           && arg1.getClass() == arg2.getClass()
           && arg1.equals(arg2))
	        return Boolean.TRUE;
        if(arg1.getClass() == Character.class
           && arg2.getClass() == Character.class
           && arg1.equals(arg2))
	        return Boolean.TRUE;
        return null;
        }

    static public Object equal(Object arg1, Object arg2) {
        if(arg1 == null)
	        return arg2 == null ? Boolean.TRUE : null;
        else if(arg2 == null)
            return null;
	    return (eql(arg1,arg2) != null
	            || (arg1.getClass() == Cons.class
	                && arg2.getClass() == Cons.class
	                && equal(((Cons)arg1).first,((Cons)arg2).first)!=null
	                && equal(((Cons)arg1).rest,((Cons)arg2).rest)!=null))
	           ?Boolean.TRUE:null;
	    }

static public Iter iter(Object coll)
	{
	if(coll == null)
		return null;
	else if(coll instanceof ISeq)
		return ((ISeq) coll).iter();
	else if(coll instanceof Iterator)
		{
		Iterator i = (Iterator) coll;
		if(i.hasNext())
			return new IteratorIter(i);
		return null;
		}
	else
		throw new IllegalArgumentException("Don't know how to create Iter from arg");
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
                               Object arg5, Cons args) throws Exception
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
		throw new IllegalArgumentException("Too many arguments to values (> ThreadLocalData.MULTIPLE_VALUES_LIMIT)");
	return arg1;
	}

}
