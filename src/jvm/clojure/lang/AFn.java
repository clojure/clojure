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

package clojure.lang;

public class AFn extends Obj implements IFn, Cloneable{

public Object invoke() throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1) throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1, Object arg2) throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3) throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object... args)
		throws Exception
	{
	return throwArity();
	}

public Object applyTo( ISeq arglist) throws Exception {
    return applyToHelper(this,  arglist);
}
static public Object applyToHelper(IFn ifn, ISeq arglist) throws Exception
	{
	switch(RT.boundedLength(arglist, 5))
		{
		case 0:
			return ifn.invoke();
		case 1:
			return ifn.invoke( arglist.first());
		case 2:
			return ifn.invoke( arglist.first()
					, (arglist = arglist.rest()).first()
			);
		case 3:
			return ifn.invoke( arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		case 4:
			return ifn.invoke( arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		case 5:
			return ifn.invoke( arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		default:
			return ifn.invoke( arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, RT.seqToArray(arglist.rest()));
		}
	}

public static  Object throwArity()
	{
	throw new IllegalArgumentException("Wrong number of args passed");
	}

public Obj withMeta(IPersistentMap meta) {
    try{
    Obj ret = (Obj) clone();
    ret._meta = meta;
    return ret;
    }
    catch(CloneNotSupportedException ignore)
        {
        return null;
        }
}
}
