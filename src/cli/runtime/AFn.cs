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

namespace org.clojure.runtime
{

public class AFn : Obj , IFn
							  {

virtual public Object invoke(ThreadLocalData tld) /*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1) /*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1, Object arg2) /*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) /*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		/*throws Exception*/
	{
	return throwArity();
	}

virtual public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)
		/*throws Exception*/
	{
	return throwArity();
	}

virtual public Object applyTo(ThreadLocalData tld, ISeq arglist) /*throws Exception*/
{
return  applyToHelper(this,tld,arglist);
}

static public Object applyToHelper(IFn ifn,ThreadLocalData tld, ISeq arglist) /*throws Exception*/
	{
	switch(RT.boundedLength(arglist, 5))
		{
		case 0:
            return ifn.invoke(tld);
		case 1:
            return ifn.invoke(tld, arglist.first());
		case 2:
            return ifn.invoke(tld, arglist.first()
					, (arglist = arglist.rest()).first()
			);
		case 3:
            return ifn.invoke(tld, arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		case 4:
            return ifn.invoke(tld, arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		case 5:
            return ifn.invoke(tld, arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
			);
		default:
            return ifn.invoke(tld, arglist.first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, (arglist = arglist.rest()).first()
					, arglist.rest());
		}
	}

static public Object throwArity()
	{
	throw new Exception("Wrong number of args passed");
	}
}
}