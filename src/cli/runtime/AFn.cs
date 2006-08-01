/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:05:37 PM */

using System;

namespace clojure.lang
{

public class AFn : Obj , IFn
							  {

virtual public Object invoke()
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19)
	{
	return throwArity();
	}
virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
	{
	return throwArity();
	}

virtual public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
							 params Object[] args)
	{
	return throwArity();
	}

virtual public Object applyTo( ISeq arglist) 
{
return  applyToHelper(this,arglist);
}

static public Object applyToHelper(IFn ifn, ISeq arglist) {
    switch (RT.boundedLength(arglist, 20))
        {
        case 0:
            return ifn.invoke();
        case 1:
            return ifn.invoke(arglist.first());
        case 2:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
            );
        case 3:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 4:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 5:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 6:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 7:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 8:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 9:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 10:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 11:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 12:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 13:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 14:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 15:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 16:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 17:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 18:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 19:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        case 20:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
            );
        default:
            return ifn.invoke(arglist.first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , (arglist = arglist.rest()).first()
                    , RT.seqToArray(arglist.rest()));
        }
}


static public Object throwArity()
	{
	throw new Exception("Wrong number of args passed");
	}
	
public override Obj withMeta(IPersistentMap meta){
    Obj ret = (Obj) MemberwiseClone();
    ret._meta = meta;
    return ret;    }
}
}