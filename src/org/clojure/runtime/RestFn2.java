/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 8:05:10 PM */

package org.clojure.runtime;

public abstract class RestFn2 extends AFn{

protected abstract Object doInvoke( Object arg1, Object arg2, ISeq rest) throws Exception;

public Object applyTo( ISeq arglist) throws Exception
	{
    switch(RT.boundedLength(arglist, 2))
        {
        case 0:
            return invoke();
        case 1:
            return invoke(arglist.first());
        case 2:
            return invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
            );
        default:
    	return doInvoke( arglist.first()
			, (arglist = arglist.rest()).first()
			, arglist.rest());

        }
	}

public Object invoke( Object arg1, Object arg2) throws Exception
	{
	return doInvoke( arg1, arg2, null);
	}

public Object invoke( Object arg1, Object arg2, Object arg3) throws Exception
	{
	return doInvoke( arg1, arg2, RT.list(arg3));
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return doInvoke( arg1, arg2, RT.list(arg3, arg4));
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return doInvoke( arg1, arg2, RT.list(arg3, arg4, arg5));
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)
		throws Exception
	{
	return doInvoke( arg1, arg2, RT.listStar(arg3, arg4, arg5, args));
	}
}

