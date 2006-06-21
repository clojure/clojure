/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 8:21:51 PM */

using System;

namespace clojure.lang
{

public abstract class RestFn4 : AFn{

    public abstract Object doInvoke( Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest)
		/*throws Exception*/;

override public Object applyTo( ISeq arglist) /*throws Exception*/
	{
    switch (RT.boundedLength(arglist, 4))
        {
        case 0:
            return invoke();
        case 1:
            return invoke( arglist.first());
        case 2:
            return invoke( arglist.first()
                    , (arglist = arglist.rest()).first()
            );
        case 3:
            return invoke( arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 4:
            return invoke( arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        default:
            return doInvoke( arglist.first()
                , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            , arglist.rest());

        }
	}

override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/
	{
	return doInvoke( arg1, arg2, arg3, arg4, null);
	}

override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		/*throws Exception*/
	{
	return doInvoke( arg1, arg2, arg3, arg4, RT.list(arg5));
	}

override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, params Object[] args)
		/*throws Exception*/
	{
	return doInvoke( arg1, arg2, arg3, arg4, RT.listStar(arg5, RT.seq(args)));
	}
}
}

