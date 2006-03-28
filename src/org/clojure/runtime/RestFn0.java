/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 7:34:25 PM */

package org.clojure.runtime;

public abstract class RestFn0 extends AFn{

protected abstract Object doInvoke(ThreadLocalData tld, Cons rest) throws Exception;

public Object applyTo(ThreadLocalData tld, Cons arglist) throws Exception
	{
	return doInvoke(tld, arglist);
	}

public Object invoke(ThreadLocalData tld) throws Exception
	{
	return doInvoke(tld, null);
	}

public Object invoke(ThreadLocalData tld, Object arg1) throws Exception
	{
	return doInvoke(tld, RT.list(arg1));
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2) throws Exception
	{
	return doInvoke(tld, RT.list(arg1, arg2));
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) throws Exception
	{
	return doInvoke(tld, RT.list(arg1, arg2, arg3));
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return doInvoke(tld, RT.list(arg1, arg2, arg3, arg4));
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return doInvoke(tld, RT.list(arg1, arg2, arg3, arg4, arg5));
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)
		throws Exception
	{
	return doInvoke(tld, RT.listStar(arg1, arg2, arg3, arg4, arg5, args));
	}
}
