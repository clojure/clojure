/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clojure.lang
{
    /// <summary>
    /// Represents an object that can be used a function.
    /// </summary>
    public interface IFn // Callable, Runnable -- are there Java equivalents? -- 
                         // there is a notion in DLR of an attribute to method when the object is used in a functional position.
    {

        object invoke();
        object invoke(object arg1);
        object invoke(object arg1, object arg2);
        object invoke(object arg1, object arg2, object arg3);
        object invoke(object arg1, object arg2, object arg3, object arg4);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10, object arg11);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10, object arg11, object arg12);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10, object arg11, object arg12, object arg13);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                      object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                      object arg15);

        // I'm not sure how far down the list we should go.  There may be some DLR limits.
        // Also the Microsoft.Func<...> delegate types only go so far.

        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19);
        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20);

        object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20,
                             params object[] args);

        object applyTo(ISeq arglist);
    }
}
