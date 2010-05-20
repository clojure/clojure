/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/
package clojure.lang;

public abstract class RestFn extends AFunction{

abstract public int getRequiredArity();

protected Object doInvoke(Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object args) throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19,
                          Object args)
		throws Exception{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19,
                          Object arg20, Object args) throws Exception{
	return null;
}


public Object applyTo(ISeq args) throws Exception{
	if(RT.boundedLength(args, getRequiredArity()) <= getRequiredArity())
		{
		return AFn.applyToHelper(this, Util.ret1(args,args = null));
		}
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(Util.ret1(args,args = null));
		case 1:
			return doInvoke(args.first()
					, Util.ret1(args.next(),args=null));
		case 2:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 3:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 4:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 5:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 6:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 7:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 8:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 9:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 10:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 11:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 12:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 13:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 14:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 15:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 16:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 17:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 18:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 19:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));
		case 20:
			return doInvoke(args.first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, (args = args.next()).first()
					, Util.ret1(args.next(),args=null));

		}
	return throwArity(-1);
}

public Object invoke() throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(null);
		default:
			return throwArity(0);
		}

}

public Object invoke(Object arg1) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null), null);
		default:
			return throwArity(1);
		}

}

public Object invoke(Object arg1, Object arg2) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null), ArraySeq.create(Util.ret1(arg2, arg2 = null)));
		case 2:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), null);
		default:
			return throwArity(2);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                                Util.ret1(arg3, arg3 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null),
			                ArraySeq.create(Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null)));
		case 2:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                ArraySeq.create(Util.ret1(arg3, arg3 = null)));
		case 3:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                null);
		default:
			return throwArity(3);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                                Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null),
			                ArraySeq.create(Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                                Util.ret1(arg4, arg4 = null)));
		case 2:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                ArraySeq.create(Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null)));
		case 3:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                ArraySeq.create(Util.ret1(arg4, arg4 = null)));
		case 4:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null), null);
		default:
			return throwArity(4);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                                Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null),
			                                Util.ret1(arg5, arg5 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null),
			                ArraySeq.create(Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                                Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null)));
		case 2:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                ArraySeq.create(Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null),
			                                Util.ret1(arg5, arg5 = null)));
		case 3:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                ArraySeq.create(Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null)));
		case 4:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null), ArraySeq.create(Util.ret1(arg5, arg5 = null)));
		case 5:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null), null);
		default:
			return throwArity(5);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                                Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null),
			                                Util.ret1(arg5, arg5 = null), Util.ret1(arg6, arg6 = null)));
		case 1:
			return doInvoke(Util.ret1(arg1, arg1 = null),
			                ArraySeq.create(Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                                Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null),
			                                Util.ret1(arg6, arg6 = null)));
		case 2:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null),
			                ArraySeq.create(Util.ret1(arg3, arg3 = null), Util.ret1(arg4, arg4 = null),
			                                Util.ret1(arg5, arg5 = null), Util.ret1(arg6, arg6 = null)));
		case 3:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                ArraySeq.create(Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null),
			                                Util.ret1(arg6, arg6 = null)));
		case 4:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null),
			                ArraySeq.create(Util.ret1(arg5, arg5 = null), Util.ret1(arg6, arg6 = null)));
		case 5:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null),
			                ArraySeq.create(Util.ret1(arg6, arg6 = null)));
		case 6:
			return doInvoke(Util.ret1(arg1, arg1 = null), Util.ret1(arg2, arg2 = null), Util.ret1(arg3, arg3 = null),
			                Util.ret1(arg4, arg4 = null), Util.ret1(arg5, arg5 = null), Util.ret1(arg6, arg6 = null),
			                null);
		default:
			return throwArity(6);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, null);
		default:
			return throwArity(7);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, null);
		default:
			return throwArity(8);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, null);
		default:
			return throwArity(9);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9, arg10));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9, arg10));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, ArraySeq.create(arg10));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, null);
		default:
			return throwArity(10);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10, arg11));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9, arg10, arg11));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9, arg10, arg11));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, ArraySeq.create(arg10, arg11));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, ArraySeq.create(arg11));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, null);
		default:
			return throwArity(11);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9, arg10, arg11, arg12));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9, arg10, arg11, arg12));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, ArraySeq.create(arg10, arg11, arg12));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, ArraySeq.create(arg11, arg12));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, ArraySeq.create(arg12));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, null);
		default:
			return throwArity(12);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(
					ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13));
		case 2:
			return doInvoke(arg1, arg2,
			                ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 3:
			return doInvoke(arg1, arg2, arg3,
			                ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4,
			                ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5,
			                ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6,
			                ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12, arg13));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, null);
		default:
			return throwArity(13);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14));
		case 3:
			return doInvoke(arg1, arg2, arg3,
			                ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4,
			                ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5,
			                ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6,
			                ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13, arg14));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13, arg14));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                null);
		default:
			return throwArity(14);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4,
			                ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5,
			                ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6,
			                ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13, arg14, arg15));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14, arg15));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, null);
		default:
			return throwArity(15);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15, arg16));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15, arg16));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15, arg16));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15, arg16));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                        arg13, arg14, arg15, arg16));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5,
			                ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6,
			                ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14, arg15, arg16));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15, arg16));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15, arg16));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15, arg16));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15, arg16));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15, arg16));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ArraySeq.create(arg16));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, null);
		default:
			return throwArity(16);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15, arg16, arg17));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15, arg16, arg17));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15, arg16, arg17));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15, arg16, arg17));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                        arg13, arg14, arg15, arg16, arg17));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                              arg13, arg14, arg15, arg16, arg17));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6,
			                ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15, arg16, arg17));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15, arg16, arg17));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15, arg16, arg17));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15, arg16, arg17));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15, arg16, arg17));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ArraySeq.create(arg16, arg17));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, ArraySeq.create(arg17));
		case 17:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, null);
		default:
			return throwArity(17);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15, arg16, arg17, arg18));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15, arg16, arg17, arg18));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15, arg16, arg17, arg18));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15, arg16, arg17, arg18));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                        arg13, arg14, arg15, arg16, arg17, arg18));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                              arg13, arg14, arg15, arg16, arg17, arg18));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12,
			                                                                    arg13, arg14, arg15, arg16, arg17,
			                                                                    arg18));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			                ArraySeq.create(arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
			                ArraySeq.create(arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15, arg16, arg17, arg18));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15, arg16, arg17, arg18));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15, arg16, arg17, arg18));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15, arg16, arg17, arg18));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ArraySeq.create(arg16, arg17, arg18));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, ArraySeq.create(arg17, arg18));
		case 17:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, ArraySeq.create(arg18));
		case 18:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, null);
		default:
			return throwArity(18);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                        arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                              arg13, arg14, arg15, arg16, arg17, arg18,
			                                                              arg19));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12,
			                                                                    arg13, arg14, arg15, arg16, arg17,
			                                                                    arg18, arg19));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9, arg10, arg11, arg12,
			                                                                          arg13, arg14, arg15, arg16, arg17,
			                                                                          arg18, arg19));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9, arg10, arg11, arg12,
			                                                                                arg13, arg14, arg15, arg16,
			                                                                                arg17, arg18, arg19));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
			                ArraySeq.create(arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15, arg16, arg17, arg18, arg19));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15, arg16, arg17, arg18, arg19));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15, arg16, arg17, arg18, arg19));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ArraySeq.create(arg16, arg17, arg18, arg19));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, ArraySeq.create(arg17, arg18, arg19));
		case 17:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, ArraySeq.create(arg18, arg19));
		case 18:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, ArraySeq.create(arg19));
		case 19:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, arg19, null);
		default:
			return throwArity(19);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 1:
			return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                      arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 2:
			return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                            arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 3:
			return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                  arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                        arg13, arg14, arg15, arg16, arg17, arg18, arg19,
			                                                        arg20));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                                                              arg13, arg14, arg15, arg16, arg17, arg18,
			                                                              arg19, arg20));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ArraySeq.create(arg7, arg8, arg9, arg10, arg11, arg12,
			                                                                    arg13, arg14, arg15, arg16, arg17,
			                                                                    arg18, arg19, arg20));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ArraySeq.create(arg8, arg9, arg10, arg11, arg12,
			                                                                          arg13, arg14, arg15, arg16, arg17,
			                                                                          arg18, arg19, arg20));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ArraySeq.create(arg9, arg10, arg11, arg12,
			                                                                                arg13, arg14, arg15, arg16,
			                                                                                arg17, arg18, arg19,
			                                                                                arg20));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, ArraySeq.create(arg10, arg11, arg12,
			                                                                                      arg13, arg14, arg15,
			                                                                                      arg16, arg17, arg18,
			                                                                                      arg19, arg20));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
			                ArraySeq.create(arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ArraySeq.create(arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ArraySeq.create(arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ArraySeq.create(arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ArraySeq.create(arg15, arg16, arg17, arg18, arg19, arg20));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ArraySeq.create(arg16, arg17, arg18, arg19, arg20));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, ArraySeq.create(arg17, arg18, arg19, arg20));
		case 17:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, ArraySeq.create(arg18, arg19, arg20));
		case 18:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, ArraySeq.create(arg19, arg20));
		case 19:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, arg19, ArraySeq.create(arg20));
		case 20:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, arg19, arg20, null);
		default:
			return throwArity(20);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20, Object... args)
		throws Exception{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ontoArrayPrepend(args, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                                 arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 1:
			return doInvoke(arg1, ontoArrayPrepend(args, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                                       arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 2:
			return doInvoke(arg1, arg2, ontoArrayPrepend(args, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                                             arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19,
			                                             arg20));
		case 3:
			return doInvoke(arg1, arg2, arg3, ontoArrayPrepend(args, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                                                   arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19,
			                                                   arg20));
		case 4:
			return doInvoke(arg1, arg2, arg3, arg4, ontoArrayPrepend(args, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                                                         arg12, arg13, arg14, arg15, arg16, arg17, arg18,
			                                                         arg19, arg20));
		case 5:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, ontoArrayPrepend(args, arg6, arg7, arg8, arg9, arg10, arg11,
			                                                               arg12, arg13, arg14, arg15, arg16, arg17,
			                                                               arg18, arg19, arg20));
		case 6:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, ontoArrayPrepend(args, arg7, arg8, arg9, arg10, arg11,
			                                                                     arg12, arg13, arg14, arg15, arg16,
			                                                                     arg17, arg18, arg19, arg20));
		case 7:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, ontoArrayPrepend(args, arg8, arg9, arg10, arg11,
			                                                                           arg12, arg13, arg14, arg15,
			                                                                           arg16, arg17, arg18, arg19,
			                                                                           arg20));
		case 8:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, ontoArrayPrepend(args, arg9, arg10, arg11,
			                                                                                 arg12, arg13, arg14, arg15,
			                                                                                 arg16, arg17, arg18, arg19,
			                                                                                 arg20));
		case 9:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, ontoArrayPrepend(args, arg10, arg11,
			                                                                                       arg12, arg13, arg14,
			                                                                                       arg15, arg16, arg17,
			                                                                                       arg18, arg19,
			                                                                                       arg20));
		case 10:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, ontoArrayPrepend(args, arg11,
			                                                                                              arg12, arg13,
			                                                                                              arg14, arg15,
			                                                                                              arg16, arg17,
			                                                                                              arg18, arg19,
			                                                                                              arg20));
		case 11:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
			                ontoArrayPrepend(args, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 12:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
			                ontoArrayPrepend(args, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 13:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
			                ontoArrayPrepend(args, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
		case 14:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                ontoArrayPrepend(args, arg15, arg16, arg17, arg18, arg19, arg20));
		case 15:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, ontoArrayPrepend(args, arg16, arg17, arg18, arg19, arg20));
		case 16:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, ontoArrayPrepend(args, arg17, arg18, arg19, arg20));
		case 17:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, ontoArrayPrepend(args, arg18, arg19, arg20));
		case 18:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, ontoArrayPrepend(args, arg19, arg20));
		case 19:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, arg19, ontoArrayPrepend(args, arg20));
		case 20:
			return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                arg15, arg16, arg17, arg18, arg19, arg20, ArraySeq.create(args));
		default:
			return throwArity(21);
		}

}


protected static ISeq ontoArrayPrepend(Object[] array, Object... args){
	ISeq ret = ArraySeq.create(array);
	for(int i = args.length - 1; i >= 0; --i)
		ret = RT.cons(args[i], ret);
	return ret;
}

protected static ISeq findKey(Object key, ISeq args){
	while(args != null)
		{
		if(key == args.first())
			return args.next();
		args = RT.next(args);
		args = RT.next(args);
		}
	return null;
}


}

