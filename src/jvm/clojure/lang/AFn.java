/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:05:37 PM */

package clojure.lang;

public abstract class AFn implements IFn {

public Object call() throws Exception{
	return invoke();
}

public void run(){
	try
		{
		invoke();
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}



public Object invoke() throws Exception{
	return throwArity();
}

public Object invoke(Object arg1) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	return throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	return throwArity();
}


public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		throws Exception{
	return throwArity();
}

public Object applyTo(ISeq arglist) throws Exception{
	return applyToHelper(this, Util.ret1(arglist,arglist = null));
}

static public Object applyToHelper(IFn ifn, ISeq arglist) throws Exception{
	switch(RT.boundedLength(arglist, 20))
		{
		case 0:
			arglist = null;
			return ifn.invoke();
		case 1:
			Object a1 = arglist.first();
			arglist = null;
			return ifn.invoke(a1);
		case 2:
			return ifn.invoke(arglist.first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 3:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 4:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 5:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 6:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 7:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 8:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 9:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 10:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 11:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 12:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 13:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 14:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 15:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 16:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 17:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 18:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 19:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		case 20:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, Util.ret1((arglist = arglist.next()).first(),arglist = null)
			);
		default:
			return ifn.invoke(arglist.first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, (arglist = arglist.next()).first()
					, RT.seqToArray(Util.ret1(arglist.next(),arglist = null)));
		}
}

public Object throwArity(){
	String name = getClass().getSimpleName();
	int suffix = name.lastIndexOf("__");
	throw new IllegalArgumentException("Wrong number of args passed to: "
	                                   + (suffix == -1 ? name : name.substring(0, suffix)).replace('_', '-'));
}
}
