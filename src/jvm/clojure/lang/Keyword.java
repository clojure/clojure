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

package clojure.lang;


public class Keyword extends Symbol implements IFn{


/**
 * Used by intern()
 *
 * @param name

 */
Keyword(String name)
	{
	super(name);
	}


public Object invoke() throws Exception {
    return AFn.throwArity();
}

/**
 *  Indexer implements IFn for attr access
 *  This single arg version is the getter
 * @param obj - must be Obj
 * @return the value of the attr or nil if not found
 * @throws Exception
 */
public Object invoke(Object obj) throws Exception
    {
    if (obj == null)
        return null;
    return ((IObj)obj).get(this);
    }

/**
 *  Indexer implements IFn for attr access
 *  This two arg version is the setter
 * @param obj - must be Obj
 * @param val
 * @return val
 * @throws Exception
 */
public Object invoke(Object obj, Object val) throws Exception
	{
	return ((IObj)obj).put(this,val);
	}

public Object invoke( Object arg1, Object arg2, Object arg3) throws Exception
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return AFn.throwArity();
	}

public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)
		throws Exception
	{
	return AFn.throwArity();
	}

public Object applyTo( ISeq arglist) throws Exception {
    return AFn.applyToHelper(this, arglist);
}
}
