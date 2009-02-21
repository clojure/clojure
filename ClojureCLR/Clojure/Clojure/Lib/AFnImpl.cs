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
    /// An implementation of AFn. Instances of this class are created by the compiler.
    /// </summary>
    /// <remarks>
    /// <para>We need this at the moment as a workaround to DLR not being able to generate instance methods from lambdas.</para>
    /// <para>Per Java Rev 1122, need to make all true functions implement the marker interface <see cref="AFunction">AFunction</see>.
    /// In the Java version this is done by making the per-funcion generated class implement the interface.  
    /// We can do that, too, once we start generating per-function classes.
    /// And then it goes away again in Rev 1161.  Sigh.</para>
    /// </remarks>
    public class AFnImpl : /*AFn*/ AFunction /* per rev 1161*/ , Fn  
    {
        public FFunc<
            object> _fn0;

        public FFunc<
            object,
            object> _fn1;
        
        public FFunc<
            object, object, 
            object> _fn2;

        public FFunc<
            object, object, object, 
            object> _fn3;
        
        public FFunc<
            object, object, object, object, 
            object> _fn4;
        
        public FFunc<
            object, object, object, object, object,
            object> _fn5;
        
        public FFunc<
            object, object, object, object, object,
            object, 
            object> _fn6;
        
        public FFunc<
            object, object, object, object, object,
            object, object, 
            object> _fn7;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object,
            object> _fn8;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, 
            object> _fn9;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object> _fn10;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, 
            object> _fn11;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, 
            object> _fn12;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, 
            object> _fn13;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, 
            object> _fn14;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object> _fn15;
       
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, 
            object> _fn16;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object,
            object> _fn17;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, 
            object> _fn18;
        
        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, 
            object> _fn19;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object> _fn20;

        public VFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object> _fnRest;


        public AFnImpl()
        {
        }

        public override object invoke()
        {
            if (_fn0 == null) throw WrongArityException();
            return _fn0();
        }
        public override object invoke(object arg1)
        {
            if (_fn1 == null) throw WrongArityException();
            return _fn1(arg1);
        }

        public override object invoke(object arg1, object arg2)
        {
            if (_fn2 == null) throw WrongArityException();
            return _fn2(arg1, arg2);
        }

        public override object invoke(object arg1, object arg2, object arg3)
        {
            if (_fn3 == null) throw WrongArityException();
            return _fn3(arg1, arg2, arg3);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            if (_fn4 == null) throw WrongArityException();
            return _fn4(arg1, arg2, arg3, arg4);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            if (_fn5 == null) throw WrongArityException();
            return _fn5(arg1, arg2, arg3, arg4, arg5);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
        {
            if (_fn6 == null) throw WrongArityException();
            return _fn6(arg1, arg2, arg3, arg4, arg5, arg6);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
        {
            if (_fn7 == null) throw WrongArityException();
            return _fn7(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8)
        {
            if (_fn8 == null) throw WrongArityException();
            return _fn8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9)
        {
            if (_fn9 == null) throw WrongArityException();
            return _fn9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10)
        {
            if (_fn10 == null) throw WrongArityException();
            return _fn10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11)
        {
            if (_fn11 == null) throw WrongArityException();
            return _fn11(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12)
        {
            if (_fn12 == null) throw WrongArityException();
            return _fn12(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13)
        {
            if (_fn13 == null) throw WrongArityException();
            return _fn13(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14)
        {
            if (_fn14 == null) throw WrongArityException();
            return _fn14(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15)
        {
            if (_fn15 == null) throw WrongArityException();
            return _fn15(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16)
        {
            if (_fn16 == null) throw WrongArityException();
            return _fn16(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17)
        {
            if (_fn17 == null) throw WrongArityException();
            return _fn17(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18)
        {
            if (_fn18 == null) throw WrongArityException();
            return _fn18(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19)
        {
            if (_fn19 == null) throw WrongArityException();
            return _fn19(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object arg20)
        {
            if (_fn20 == null) throw WrongArityException();
            return _fn20(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object arg20, params object[] args)
        {
            if (_fnRest == null) throw WrongArityException();
            return _fnRest(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
        }
    }

}
