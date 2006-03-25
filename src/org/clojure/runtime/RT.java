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

public class RT{

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
}
