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

    // The problem here is that I need the functionality of both RestFn and AfnImpl.
    // Because they are both classes, we can't derive from both.
    // For the time being, I choose to inherit from RestFn and re-implement the AFnImpl code.
    // Eventually, we need to do overloading to solve this problem.
    // Overloading is not possible at the moment do to a bug in LambdaExpression.CompileToMethod

    public class RestFnImpl : RestFn, Fn
    // Per Java Rev 1122, need to make all true functions implement this marker interface, 
    // In java version this is done by making the per-funcion generated class implement the interface.  
    // We can do that, too, once we start generating per-function classes.
    // And then it goes away again in Rev 1161.  Sigh.  (RestFn implements AFunction, which apparently suffices.)
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


        public FFunc<
             object, object> _fnDo0;

        public FFunc<
            object,
            object, object> _fnDo1;

        public FFunc<
            object, object,
            object, object> _fnDo2;

        public FFunc<
            object, object, object,
            object, object> _fnDo3;

        public FFunc<
            object, object, object, object,
            object, object> _fnDo4;

        public FFunc<
            object, object, object, object, object,
            object, object> _fnDo5;

        public FFunc<
            object, object, object, object, object,
            object,
            object, object> _fnDo6;

        public FFunc<
            object, object, object, object, object,
            object, object,
            object, object> _fnDo7;

        public FFunc<
            object, object, object, object, object,
            object, object, object,
            object, object> _fnDo8;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object,
            object, object> _fnDo9;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object> _fnDo10;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object,
            object, object> _fnDo11;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object,
            object, object> _fnDo12;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object,
            object, object> _fnDo13;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object,
            object, object> _fnDo14;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object> _fnDo15;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object,
            object, object> _fnDo16;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object,
            object, object> _fnDo17;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object,
            object, object> _fnDo18;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object,
            object, object> _fnDo19;

        public FFunc<
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object, object, object, object,
            object, object> _fnDo20;

        public RestFnImpl(int reqArity)
            : base(reqArity)
        {
        }




        protected override object doInvoke(object args)
        {
            if (_fnDo0 == null) throw WrongArityException();
            return _fnDo0(args);
        }
        protected override object doInvoke(object arg1, object args)
        {
            if (_fnDo1 == null) throw WrongArityException();
            return _fnDo1(arg1, args);
        }

        protected override object doInvoke(object arg1, object arg2, object args)
        {
            if (_fnDo2 == null) throw WrongArityException();
            return _fnDo2(arg1, arg2, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object args)
        {
            if (_fnDo3 == null) throw WrongArityException();
            return _fnDo3(arg1, arg2, arg3, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object args)
        {
            if (_fnDo4 == null) throw WrongArityException();
            return _fnDo4(arg1, arg2, arg3, arg4, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object args)
        {
            if (_fnDo5 == null) throw WrongArityException();
            return _fnDo5(arg1, arg2, arg3, arg4, arg5, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object args)
        {
            if (_fnDo6 == null) throw WrongArityException();
            return _fnDo6(arg1, arg2, arg3, arg4, arg5, arg6, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object args)
        {
            if (_fnDo7 == null) throw WrongArityException();
            return _fnDo7(arg1, arg2, arg3, arg4, arg5, arg6, arg7, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object args)
        {
            if (_fnDo8 == null) throw WrongArityException();
            return _fnDo8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object args)
        {
            if (_fnDo9 == null) throw WrongArityException();
            return _fnDo9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object args)
        {
            if (_fnDo10 == null) throw WrongArityException();
            return _fnDo10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object args)
        {
            if (_fnDo11 == null) throw WrongArityException();
            return _fnDo11(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object args)
        {
            if (_fnDo12 == null) throw WrongArityException();
            return _fnDo12(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object args)
        {
            if (_fnDo13 == null) throw WrongArityException();
            return _fnDo13(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object args)
        {
            if (_fnDo14 == null) throw WrongArityException();
            return _fnDo14(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object args)
        {
            if (_fnDo15 == null) throw WrongArityException();
            return _fnDo15(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object args)
        {
            if (_fnDo16 == null) throw WrongArityException();
            return _fnDo16(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object args)
        {
            if (_fnDo17 == null) throw WrongArityException();
            return _fnDo17(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object args)
        {
            if (_fnDo18 == null) throw WrongArityException();
            return _fnDo18(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object args)
        {
            if (_fnDo19 == null) throw WrongArityException();
            return _fnDo19(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, args);
        }

        protected override object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object arg20, object args)
        {
            if (_fnDo20 == null) throw WrongArityException();
            return _fnDo20(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
        }

        public override object invoke()
        {
            return (_fn0 == null)
                ? base.invoke()
                : _fn0();
        }
        public override object invoke(object arg1)
        {
            return (_fn1 == null)
                ? base.invoke(arg1)
                : _fn1(arg1);
        }

        public override object invoke(object arg1, object arg2)
        {
            return (_fn2 == null)
                ? base.invoke(arg1, arg2)
                : _fn2(arg1, arg2);
        }

        public override object invoke(object arg1, object arg2, object arg3)
        {
            return (_fn3 == null)
                ? base.invoke(arg1, arg2, arg3)
                : _fn3(arg1, arg2, arg3);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            return (_fn4 == null)
                ? base.invoke(arg1, arg2, arg3, arg4)
                : _fn4(arg1, arg2, arg3, arg4);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return (_fn5 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5)
                : _fn5(arg1, arg2, arg3, arg4, arg5);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
        {
            return (_fn6 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6)
                : _fn6(arg1, arg2, arg3, arg4, arg5, arg6);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
        {
            return (_fn7 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
                : _fn7(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8)
        {
            return (_fn8 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
                : _fn8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9)
        {
            return (_fn9 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
                : _fn9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10)
        {
            return (_fn10 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
                : _fn10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11)
        {
            return (_fn11 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
                : _fn11(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12)
        {
            return (_fn12 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
                : _fn12(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13)
        {
            return (_fn13 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
                : _fn13(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14)
        {
            return (_fn14 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
                : _fn14(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15)
        {
            return (_fn15 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
                : _fn15(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16)
        {
            return (_fn16 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
                : _fn16(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17)
        {
            return (_fn17 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
                : _fn17(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18)
        {
            return (_fn18 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
                : _fn18(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19)
        {
            return (_fn19 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
                : _fn19(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object arg20)
        {
            return (_fn20 == null)
                ? base.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
                : _fn20(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
        }

        //public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, object arg18, object arg19, object arg20, params object[] args)
        //{
        //    return (_fnRest == null) ? base.invoke() 
        //    : _fnRest(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
        //}
    }
}
