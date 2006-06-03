/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2006 */

package org.clojure.runtime;

public class Accessor extends Symbol implements IFn{

String memberName;
Accessor(String name)
	{
        super(name);
        memberName = name.substring(1);
    }


public Object invoke(ThreadLocalData tld) throws Exception {
    return AFn.throwArity();
}
/**
 *  Indexer implements IFn for attr access
 *  This single arg version is the getter
 * @param tld
 * @param obj - must be Obj
 * @return the value of the attr or nil if not found
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj) throws Exception
	{

    return Reflector.invokeInstanceMember(memberName,obj);
	}

/**
 *  Indexer implements IFn for attr access
 *  This two arg version is the setter
 * @param tld
 * @param obj - must be Obj
 * @param val
 * @return val
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj, Object val) throws Exception
	{

	return Reflector.invokeInstanceMember(memberName,obj,val);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) throws Exception
	{
	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4,arg5);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)
		throws Exception
	{
	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4,arg5,args);
	}

public Object applyTo(ThreadLocalData tld, Cons arglist) throws Exception {
    return AFn.applyToHelper(this, tld, arglist);
}

}
