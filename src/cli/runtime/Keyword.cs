/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 29, 2006 10:39:05 AM */

using System;


namespace clojure.lang
{

public class Keyword : Symbol, IFn{


internal Keyword(String name):base(name)	{	}public Object invoke() /*throws Exception*/ {
    return AFn.throwArity();
}/** *  Indexer implements IFn for attr access *  This single arg version is the getter * @param tld * @param obj - must be AMap * @return the value of the attr or nil if not found */public Object invoke( Object obj) /*throws Exception*/	{
    if (obj == null)
        return null;
    return ((IObj)obj).get(this);	}/** *  Indexer implements IFn for attr access *  This two arg version is the setter * @param tld * @param obj - must be AMap * @param val * @return val */public Object invoke( Object obj, Object val) /*throws Exception*/	{	return ((IObj)obj).put(this,val);	}

public Object invoke( Object arg1, Object arg2, Object arg3) /*throws Exception*/
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		/*throws Exception*/
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)
		/*throws Exception*/
	{
	return AFn.throwArity();
	}

public Object applyTo( ISeq arglist) /*throws Exception*/ {
    return AFn.applyToHelper(this,  arglist);
}
}
}