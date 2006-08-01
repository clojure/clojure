/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public abstract class RestFn extends AFn {

protected int reqArity;

protected abstract Object doInvoke(ISeq args) throws Exception;

RestFn(int reqArity) {
    this.reqArity = reqArity;
}

public Object applyTo(ISeq args) throws Exception {
    if (RT.boundedLength(args, reqArity) <= reqArity)
        {
        return AFn.applyToHelper(this, args);
        }
    return doInvoke(args);
}

public Object invoke() throws Exception {
    return doInvoke(null);
}

public Object invoke(Object arg1) throws Exception {
    if (reqArity > 1)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1));
}

public Object invoke(Object arg1, Object arg2) throws Exception {
    if (reqArity > 2)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2));
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception {
    if (reqArity > 3)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception {
    if (reqArity > 4)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception {
    if (reqArity > 5)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception {
    if (reqArity > 6)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
        throws Exception {
    if (reqArity > 7)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception {
    if (reqArity > 8)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception {
    if (reqArity > 9)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception {
    if (reqArity > 10)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception {
    if (reqArity > 11)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception {
    if (reqArity > 12)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
        throws Exception {
    if (reqArity > 13)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
        throws Exception {
    if (reqArity > 14)
        return throwArity();
    return doInvoke(
            ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception {
    if (reqArity > 15)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception {
    if (reqArity > 16)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15, arg16));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception {
    if (reqArity > 17)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15, arg16, arg17));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception {
    if (reqArity > 18)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15, arg16, arg17, arg18));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception {
    if (reqArity > 19)
        return throwArity();
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15, arg16, arg17, arg18, arg19));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
        throws Exception {
    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    arg14, arg15, arg16, arg17, arg18, arg19, arg20));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
        throws Exception {
    Object[] arguments = new Object[20 + args.length];
	arguments[0] = arg1;
	arguments[1] = arg2;
	arguments[2] = arg3;
    arguments[3] = arg4;
    arguments[4] = arg5;
    arguments[5] = arg6;
    arguments[6] = arg7;
    arguments[7] = arg8;
    arguments[8] = arg9;
    arguments[9] = arg10;
    arguments[10] = arg11;
    arguments[11] = arg12;
    arguments[12] = arg13;
    arguments[13] = arg14;
    arguments[14] = arg15;
    arguments[15] = arg16;
    arguments[16] = arg17;
    arguments[17] = arg18;
    arguments[18] = arg19;
    arguments[19] = arg20;
    System.arraycopy(args, 0, arguments, 20, args.length);
    return doInvoke(ArraySeq.create(arguments));
}

}

