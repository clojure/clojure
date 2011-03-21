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

protected Object doInvoke(Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object args) {
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19,
                          Object args)
		{
	return null;
}

protected Object doInvoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                          Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
                          Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19,
                          Object arg20, Object args) {
	return null;
}


public Object applyTo(ISeq args) {
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

public Object invoke() {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(null);
		default:
			return throwArity(0);
		}

}

public Object invoke(Object arg1) {
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

public Object invoke(Object arg1, Object arg2) {
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

public Object invoke(Object arg1, Object arg2, Object arg3) {
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

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
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

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
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

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
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
		{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1,arg1=null), 
                                                        Util.ret1(arg2,arg2=null), 
                                                        Util.ret1(arg3,arg3=null), 
                                                        Util.ret1(arg4,arg4=null), 
                                                        Util.ret1(arg5,arg5=null), 
                                                        Util.ret1(arg6,arg6=null), 
                                                        Util.ret1(arg7,arg7=null)));
		case 1:
			return doInvoke(Util.ret1(arg1,arg1=null), ArraySeq.create(Util.ret1(arg2,arg2=null), 
                                                                                   Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null)));
		case 2:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), ArraySeq.create(Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), ArraySeq.create(Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ArraySeq.create(Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), ArraySeq.create(Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), ArraySeq.create(Util.ret1(arg7,arg7=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), null);
		default:
			return throwArity(7);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1,arg1=null), 
                                                        Util.ret1(arg2,arg2=null), 
                                                        Util.ret1(arg3,arg3=null), 
                                                        Util.ret1(arg4,arg4=null), 
                                                        Util.ret1(arg5,arg5=null), 
                                                        Util.ret1(arg6,arg6=null), 
                                                        Util.ret1(arg7,arg7=null), 
                                                        Util.ret1(arg8,arg8=null)));
		case 1:
			return doInvoke(Util.ret1(arg1,arg1=null), ArraySeq.create(Util.ret1(arg2,arg2=null), 
                                                                                   Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 2:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), ArraySeq.create(Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), ArraySeq.create(Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ArraySeq.create(Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), ArraySeq.create(Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), ArraySeq.create(Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), ArraySeq.create(Util.ret1(arg8,arg8=null)));
		case 8:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), 
                                        Util.ret1(arg8,arg8=null), null);
		default:
			return throwArity(8);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1,arg1=null), 
                                                        Util.ret1(arg2,arg2=null), 
                                                        Util.ret1(arg3,arg3=null), 
                                                        Util.ret1(arg4,arg4=null), 
                                                        Util.ret1(arg5,arg5=null), 
                                                        Util.ret1(arg6,arg6=null), 
                                                        Util.ret1(arg7,arg7=null), 
                                                        Util.ret1(arg8,arg8=null), 
                                                        Util.ret1(arg9,arg9=null)));
		case 1:
			return doInvoke(Util.ret1(arg1,arg1=null), ArraySeq.create(Util.ret1(arg2,arg2=null), 
                                                                                   Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 2:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), ArraySeq.create(Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), ArraySeq.create(Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ArraySeq.create(Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), ArraySeq.create(Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), ArraySeq.create(Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), ArraySeq.create(Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null)));
		case 8:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), 
                                        Util.ret1(arg8,arg8=null), ArraySeq.create(Util.ret1(arg9,arg9=null)));
		case 9:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), 
                                        Util.ret1(arg8,arg8=null), 
                                        Util.ret1(arg9,arg9=null), null);
		default:
			return throwArity(9);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(Util.ret1(arg1,arg1=null), 
                                                        Util.ret1(arg2,arg2=null), 
                                                        Util.ret1(arg3,arg3=null), 
                                                        Util.ret1(arg4,arg4=null), 
                                                        Util.ret1(arg5,arg5=null), 
                                                        Util.ret1(arg6,arg6=null), 
                                                        Util.ret1(arg7,arg7=null), 
                                                        Util.ret1(arg8,arg8=null), 
                                                        Util.ret1(arg9,arg9=null), 
                                                        Util.ret1(arg10,arg10=null)));
		case 1:
			return doInvoke(Util.ret1(arg1,arg1=null), ArraySeq.create(Util.ret1(arg2,arg2=null), 
                                                                                   Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 2:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), ArraySeq.create(Util.ret1(arg3,arg3=null), 
                                                                                   Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), ArraySeq.create(Util.ret1(arg4,arg4=null), 
                                                                                   Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ArraySeq.create(Util.ret1(arg5,arg5=null), 
                                                                                   Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), ArraySeq.create(Util.ret1(arg6,arg6=null), 
                                                                                   Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), ArraySeq.create(Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), ArraySeq.create(Util.ret1(arg8,arg8=null), 
                                                                                   Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 8:
			return doInvoke(Util.ret1(arg1,arg1=null), 
                                        Util.ret1(arg2,arg2=null), 
                                        Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), 
                                        Util.ret1(arg5,arg5=null), 
                                        Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), 
                                        Util.ret1(arg8,arg8=null), ArraySeq.create(Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), ArraySeq.create(
 Util.ret1(arg10,arg10=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), null);
		default:
			return throwArity(10);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), ArraySeq.create(
 Util.ret1(arg11,arg11=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), null);
		default:
			return throwArity(11);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), ArraySeq.create(
 Util.ret1(arg12,arg12=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), null);
		default:
			return throwArity(12);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(
					ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null),
			                ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null),
			                ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null),
			                ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null),
			                ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null),
			                ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), null);
		default:
			return throwArity(13);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null),
			                ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null),
			                ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null),
			                ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null),
			                ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                null);
		default:
			return throwArity(14);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                  
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null),
			                ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null),
			                ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null),
			                ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                ArraySeq.create(
 Util.ret1(arg15,arg15=null)));
		case 15:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), null);
		default:
			return throwArity(15);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                  
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                        
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null),
			                ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null),
			                ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                ArraySeq.create(
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null)));
		case 15:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), ArraySeq.create(
 Util.ret1(arg16,arg16=null)));
		case 16:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), null);
		default:
			return throwArity(16);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                  
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                        
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                              
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null),
			                ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                ArraySeq.create(
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 15:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), ArraySeq.create(
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null)));
		case 16:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), ArraySeq.create(
 Util.ret1(arg17,arg17=null)));
		case 17:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), null);
		default:
			return throwArity(17);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                  
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                        
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                              
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                                    
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null),
			                                                                    
 Util.ret1(arg18,arg18=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null),
			                ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null),
			                ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                ArraySeq.create(
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 15:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), ArraySeq.create(
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 16:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), ArraySeq.create(
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null)));
		case 17:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), ArraySeq.create(
 Util.ret1(arg18,arg18=null)));
		case 18:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), null);
		default:
			return throwArity(18);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 1:
                    ISeq packed = PersistentList.EMPTY;
			return doInvoke(
                                        Util.ret1(arg1,arg1=null), ArraySeq.create(Util.ret1(arg2,arg2=null),
                    Util.ret1(arg3,arg3=null),
                    Util.ret1(arg4,arg4=null),
                    Util.ret1(arg5,arg5=null),
                    Util.ret1(arg6,arg6=null),
                    Util.ret1(arg7,arg7=null),
                    Util.ret1(arg8,arg8=null),
                    Util.ret1(arg9,arg9=null),
                    Util.ret1(arg10,arg10=null),
                    Util.ret1(arg11,arg11=null),
                    Util.ret1(arg12,arg12=null),
                    Util.ret1(arg13,arg13=null),
                    Util.ret1(arg14,arg14=null),
                    Util.ret1(arg15,arg15=null),
                    Util.ret1(arg16,arg16=null),
                    Util.ret1(arg17,arg17=null),
                    Util.ret1(arg18,arg18=null),
                    Util.ret1(arg19,arg19=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 3:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), ArraySeq.create(
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                  
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 4:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), ArraySeq.create(
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                        
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 5:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), ArraySeq.create(
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                              
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null),
			                                                              
 Util.ret1(arg19,arg19=null)));
		case 6:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), ArraySeq.create(
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                                    
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null),
			                                                                    
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 7:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), ArraySeq.create(
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                                          
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null),
			                                                                          
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 8:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), ArraySeq.create(
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                                                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null),
			                                                                                
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 9:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null),
			                ArraySeq.create(
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 10:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null),
			                ArraySeq.create(
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 11:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null),
			                ArraySeq.create(
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 12:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                ArraySeq.create(
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 13:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null),
			                ArraySeq.create(
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 14:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                ArraySeq.create(
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 15:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), ArraySeq.create(
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 16:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), ArraySeq.create(
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 17:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), ArraySeq.create(
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null)));
		case 18:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), ArraySeq.create(
 Util.ret1(arg19,arg19=null)));
		case 19:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null), 
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null),
			                
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null), null);
		default:
			return throwArity(19);
		}

}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ArraySeq.create(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null), 
 Util.ret1(arg20,arg20=null)));
		case 1:
			return doInvoke(
 Util.ret1(arg1,arg1=null), ArraySeq.create(
 Util.ret1(arg2,arg2=null), 
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                      
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null), 
 Util.ret1(arg20,arg20=null)));
		case 2:
			return doInvoke(
 Util.ret1(arg1,arg1=null), 
 Util.ret1(arg2,arg2=null), ArraySeq.create(
 Util.ret1(arg3,arg3=null), 
 Util.ret1(arg4,arg4=null), 
 Util.ret1(arg5,arg5=null), 
 Util.ret1(arg6,arg6=null), 
 Util.ret1(arg7,arg7=null), 
 Util.ret1(arg8,arg8=null), 
 Util.ret1(arg9,arg9=null), 
 Util.ret1(arg10,arg10=null), 
 Util.ret1(arg11,arg11=null), 
 Util.ret1(arg12,arg12=null),
			                                            
 Util.ret1(arg13,arg13=null), 
 Util.ret1(arg14,arg14=null), 
 Util.ret1(arg15,arg15=null), 
 Util.ret1(arg16,arg16=null), 
 Util.ret1(arg17,arg17=null), 
 Util.ret1(arg18,arg18=null), 
 Util.ret1(arg19,arg19=null), 
 Util.ret1(arg20,arg20=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        ArraySeq.create(Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
                                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ArraySeq.create(Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                   Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), Util.ret1(arg10,arg10=null), 
                                                                                   Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                   Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                   Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null),
                                                                                   Util.ret1(arg20,arg20=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), ArraySeq.create(Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                                              Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                                              Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), 
                                                                                                              Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                                              Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                                              Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                                              Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                                              Util.ret1(arg20,arg20=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        ArraySeq.create(Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
                                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), ArraySeq.create(Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                   Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), 
                                                                                   Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                   Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                   Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null),
                                                                                   Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                   Util.ret1(arg20,arg20=null)));
		case 8:
			return doInvoke(
                                        Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), ArraySeq.create(Util.ret1(arg9,arg9=null), Util.ret1(arg10,arg10=null), 
                                                                                                              Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
                                                                                                              Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), 
                                                                                                              Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null),
                                                                                                              Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                                              Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 9:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        ArraySeq.create(Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
                                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null),
                                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null),
                                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 10:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), ArraySeq.create(Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                                                     Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), 
                                                                                     Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                     Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                     Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 11:
			return doInvoke(
                                        Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), ArraySeq.create(Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                                                  Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                                                  Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                                                  Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
 Util.ret1(arg20,arg20=null)));
		case 12:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
			                ArraySeq.create(Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 13:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), ArraySeq.create(Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                     Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                     Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                     Util.ret1(arg20,arg20=null)));
		case 14:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), ArraySeq.create(Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                                                  Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                                                  Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 15:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null),
                                        ArraySeq.create(Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 16:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), ArraySeq.create(Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                     Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 17:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), ArraySeq.create(Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                                                  Util.ret1(arg20,arg20=null)));
		case 18:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        ArraySeq.create(Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 19:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        Util.ret1(arg19,arg19=null), ArraySeq.create(Util.ret1(arg20,arg20=null)));
		case 20:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null), null);
		default:
			return throwArity(20);
		}
        
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20, Object... args)
		{
	switch(getRequiredArity())
		{
		case 0:
			return doInvoke(ontoArrayPrepend(args, Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                                         Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                                         Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null),
                                                         Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                         Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null),
                                                         Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 1:
			return doInvoke(Util.ret1(arg1,arg1=null), ontoArrayPrepend(args, Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                                                                    Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), 
                                                                                    Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                    Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                    Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null),
                                                                                    Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                    Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                    Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                    Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                    Util.ret1(arg20,arg20=null)));
		case 2:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), ontoArrayPrepend(args, Util.ret1(arg3,arg3=null), 
                                                                                                               Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), 
                                                                                                               Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                                               Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                                               Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null),
                                                                                                               Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                                               Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                                               Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                                               Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null),
                                                                                                               Util.ret1(arg20,arg20=null)));
		case 3:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        ontoArrayPrepend(args, Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                                         Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                         Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                         Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                         Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 4:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), ontoArrayPrepend(args, Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                    Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), Util.ret1(arg10,arg10=null), 
                                                                                    Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null),
                                                                                    Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                    Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                    Util.ret1(arg20,arg20=null)));
		case 5:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null),
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), ontoArrayPrepend(args, Util.ret1(arg6,arg6=null), Util.ret1(arg7,arg7=null), 
                                                                                                               Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                                               Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null),
                                                                                                               Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                                               Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                                               Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null),
                                                                                                               Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                                               Util.ret1(arg20,arg20=null)));
		case 6:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        ontoArrayPrepend(args, Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                         Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                         Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                         Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 7:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null),
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), ontoArrayPrepend(args, Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                                                                    Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null),
                                                                                    Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                    Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null),
                                                                                    Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                    Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null),
                                                                                    Util.ret1(arg20,arg20=null)));
		case 8:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), ontoArrayPrepend(args, Util.ret1(arg9,arg9=null), Util.ret1(arg10,arg10=null), 
                                                                                                               Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                                                                               Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), 
                                                                                                               Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                                               Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                                               Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 9:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        ontoArrayPrepend(args, Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                         Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                         Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 10:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), ontoArrayPrepend(args, Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                                                                      Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), 
                                                                                      Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                      Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                      Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 11:
			return doInvoke(
                                        Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), ontoArrayPrepend(args, Util.ret1(arg12,arg12=null), Util.ret1(arg13,arg13=null), 
                                                                                                                   Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                                                   Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                                                   Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                                                   Util.ret1(arg20,arg20=null)));
		case 12:
			return doInvoke(
                                        Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null),
                                        ontoArrayPrepend(args, Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                         Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 13:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), ontoArrayPrepend(args, Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                                                                      Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), 
                                                                                      Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                      Util.ret1(arg20,arg20=null)));
		case 14:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), ontoArrayPrepend(args, Util.ret1(arg15,arg15=null), Util.ret1(arg16,arg16=null), 
                                                                                                                   Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                                                   Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 15:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        ontoArrayPrepend(args, Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                         Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 16:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), ontoArrayPrepend(args, Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                                                                      Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 17:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), ontoArrayPrepend(args, Util.ret1(arg18,arg18=null), Util.ret1(arg19,arg19=null), 
                                                                                                                   Util.ret1(arg20,arg20=null)));
		case 18:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        ontoArrayPrepend(args, Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null)));
		case 19:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        Util.ret1(arg19,arg19=null), ontoArrayPrepend(args, Util.ret1(arg20,arg20=null)));
		case 20:
			return doInvoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null), 
                                        Util.ret1(arg4,arg4=null), Util.ret1(arg5,arg5=null), Util.ret1(arg6,arg6=null), 
                                        Util.ret1(arg7,arg7=null), Util.ret1(arg8,arg8=null), Util.ret1(arg9,arg9=null), 
                                        Util.ret1(arg10,arg10=null), Util.ret1(arg11,arg11=null), Util.ret1(arg12,arg12=null), 
                                        Util.ret1(arg13,arg13=null), Util.ret1(arg14,arg14=null), Util.ret1(arg15,arg15=null), 
                                        Util.ret1(arg16,arg16=null), Util.ret1(arg17,arg17=null), Util.ret1(arg18,arg18=null), 
                                        Util.ret1(arg19,arg19=null), Util.ret1(arg20,arg20=null), ArraySeq.create(args));
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

