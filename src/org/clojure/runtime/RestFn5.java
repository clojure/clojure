/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 8:24:31 PM */

package org.clojure.runtime;

public abstract class RestFn5 extends AFn{

protected abstract Object doInvoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5,
                                   Cons rest)
		throws Exception;

public Object applyTo(ThreadLocalData tld, Cons arglist) throws Exception
	{
    switch(RT.boundedLength(arglist, 5))
        {
        case 0:
            return invoke(tld);
        case 1:
            return invoke(tld,arglist.first);
        case 2:
            return invoke(tld,arglist.first
                    , (arglist = arglist.rest).first
            );
        case 3:
            return invoke(tld,arglist.first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
            );
        case 4:
            return invoke(tld,arglist.first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
            );
        case 5:
            return invoke(tld,arglist.first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
            );
        default:
    	    return doInvoke(tld, arglist.first
                , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
                    , (arglist = arglist.rest).first
			, arglist.rest);

        }
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return doInvoke(tld, arg1, arg2, arg3, arg4, arg5, null);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)
		throws Exception
	{
	return doInvoke(tld, arg1, arg2, arg3, arg4, arg5, args);
	}
}
