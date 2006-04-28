/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 8:00:28 PM */

using System;

namespace org.clojure.runtime
{

public abstract class RestFn1 : AFn{

public abstract Object doInvoke(ThreadLocalData tld, Object arg1, Cons rest) /*throws Exception*/;

override public Object applyTo(ThreadLocalData tld, Cons arglist) /*throws Exception*/
	{
	if(arglist == null)
		throwArity();
	return doInvoke(tld, arglist.first
			, arglist.rest);
	}

override public Object invoke(ThreadLocalData tld, Object arg1) /*throws Exception*/
	{
	return doInvoke(tld, arg1, null);
	}

override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2) /*throws Exception*/
	{
	return doInvoke(tld, arg1, RT.list(arg2));
	}

override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) /*throws Exception*/
	{
	return doInvoke(tld, arg1, RT.list(arg2, arg3));
	}

override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/
	{
	return doInvoke(tld, arg1, RT.list(arg2, arg3, arg4));
	}

override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		/*throws Exception*/
	{
	return doInvoke(tld, arg1, RT.list(arg2, arg3, arg4, arg5));
	}

override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)
		/*throws Exception*/
	{
	return doInvoke(tld, arg1, RT.listStar(arg2, arg3, arg4, arg5, args));
	}
}

}