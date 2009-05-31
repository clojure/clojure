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
    public abstract class RestFn : AFunction
    {
        #region Data

        protected readonly int _reqArity;

        #endregion

        #region C-tors

        public RestFn(int reqArity)
        {
            _reqArity = reqArity;
        }

        #endregion

        #region Invokes with explicit rest arg

        protected virtual object doInvoke(object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object arg16, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object arg16, object arg17, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object arg16, object arg17, object arg18, object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object arg16, object arg17, object arg18, object arg19,
                                  object args)
        {
            return null;
        }

        protected virtual object doInvoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                                  object arg8, object arg9, object arg10, object arg11, object arg12, object arg13,
                                  object arg14, object arg15, object arg16, object arg17, object arg18, object arg19,
                                  object arg20, object args)
        {
            return null;
        }

        #endregion

        #region IFn members

        public override object applyTo(ISeq args)
        {
            if (RT.BoundedLength(args, _reqArity) <= _reqArity)
                return base.applyTo(args);

            switch (_reqArity)
            {
                case 0:
                    return doInvoke(args);
                case 1:
                    return doInvoke(args.first()
                            , args.next());
                case 2:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , args.next());
                case 3:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
                case 4:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
                case 5:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
                case 6:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
                case 7:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
                case 8:
                    return doInvoke(args.first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , (args = args.next()).first()
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());
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
                            , args.next());

            }
            throw WrongArityException();
        }


        public override Object invoke()
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1));
                case 1:
                    return doInvoke(arg1, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1, arg2));
                case 1:
                    return doInvoke(arg1, ArraySeq.create(arg2));
                case 2:
                    return doInvoke(arg1, arg2, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1, arg2, arg3));
                case 1:
                    return doInvoke(arg1, ArraySeq.create(arg2, arg3));
                case 2:
                    return doInvoke(arg1, arg2, ArraySeq.create(arg3));
                case 3:
                    return doInvoke(arg1, arg2, arg3, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4));
                case 1:
                    return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4));
                case 2:
                    return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4));
                case 3:
                    return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4));
                case 4:
                    return doInvoke(arg1, arg2, arg3, arg4, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5));
                case 1:
                    return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5));
                case 2:
                    return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5));
                case 3:
                    return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5));
                case 4:
                    return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5));
                case 5:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(ArraySeq.create(arg1, arg2, arg3, arg4, arg5, arg6));
                case 1:
                    return doInvoke(arg1, ArraySeq.create(arg2, arg3, arg4, arg5, arg6));
                case 2:
                    return doInvoke(arg1, arg2, ArraySeq.create(arg3, arg4, arg5, arg6));
                case 3:
                    return doInvoke(arg1, arg2, arg3, ArraySeq.create(arg4, arg5, arg6));
                case 4:
                    return doInvoke(arg1, arg2, arg3, arg4, ArraySeq.create(arg5, arg6));
                case 5:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, ArraySeq.create(arg6));
                case 6:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, null);
                default:
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16, Object arg17)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16, Object arg17, Object arg18)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16, Object arg17, Object arg18, Object arg19)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
        {
            switch (_reqArity)
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
                    throw WrongArityException();
            }

        }

        public override Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                             Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                             Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20, params object[] args)
        {
            switch (_reqArity)
            {
                case 0:
                    return doInvoke(OntoArrayPrepend(args, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                                     arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
                case 1:
                    return doInvoke(arg1, OntoArrayPrepend(args, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                                           arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
                case 2:
                    return doInvoke(arg1, arg2, OntoArrayPrepend(args, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                                                 arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19,
                                                                 arg20));
                case 3:
                    return doInvoke(arg1, arg2, arg3, OntoArrayPrepend(args, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                                                       arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19,
                                                                       arg20));
                case 4:
                    return doInvoke(arg1, arg2, arg3, arg4, OntoArrayPrepend(args, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                                                             arg12, arg13, arg14, arg15, arg16, arg17, arg18,
                                                                             arg19, arg20));
                case 5:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, OntoArrayPrepend(args, arg6, arg7, arg8, arg9, arg10, arg11,
                                                                                   arg12, arg13, arg14, arg15, arg16, arg17,
                                                                                   arg18, arg19, arg20));
                case 6:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, OntoArrayPrepend(args, arg7, arg8, arg9, arg10, arg11,
                                                                                         arg12, arg13, arg14, arg15, arg16,
                                                                                         arg17, arg18, arg19, arg20));
                case 7:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, OntoArrayPrepend(args, arg8, arg9, arg10, arg11,
                                                                                               arg12, arg13, arg14, arg15,
                                                                                               arg16, arg17, arg18, arg19,
                                                                                               arg20));
                case 8:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, OntoArrayPrepend(args, arg9, arg10, arg11,
                                                                                                     arg12, arg13, arg14, arg15,
                                                                                                     arg16, arg17, arg18, arg19,
                                                                                                     arg20));
                case 9:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, OntoArrayPrepend(args, arg10, arg11,
                                                                                                           arg12, arg13, arg14,
                                                                                                           arg15, arg16, arg17,
                                                                                                           arg18, arg19,
                                                                                                           arg20));
                case 10:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, OntoArrayPrepend(args, arg11,
                                                                                                                  arg12, arg13,
                                                                                                                  arg14, arg15,
                                                                                                                  arg16, arg17,
                                                                                                                  arg18, arg19,
                                                                                                                  arg20));
                case 11:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                                    OntoArrayPrepend(args, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
                case 12:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
                                    OntoArrayPrepend(args, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
                case 13:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                                    OntoArrayPrepend(args, arg14, arg15, arg16, arg17, arg18, arg19, arg20));
                case 14:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    OntoArrayPrepend(args, arg15, arg16, arg17, arg18, arg19, arg20));
                case 15:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, OntoArrayPrepend(args, arg16, arg17, arg18, arg19, arg20));
                case 16:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, arg16, OntoArrayPrepend(args, arg17, arg18, arg19, arg20));
                case 17:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, arg16, arg17, OntoArrayPrepend(args, arg18, arg19, arg20));
                case 18:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, arg16, arg17, arg18, OntoArrayPrepend(args, arg19, arg20));
                case 19:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, arg16, arg17, arg18, arg19, OntoArrayPrepend(args, arg20));
                case 20:
                    return doInvoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                    arg15, arg16, arg17, arg18, arg19, arg20, ArraySeq.create(args));
                default:
                    throw WrongArityException();
            }

        }


        protected static ISeq OntoArrayPrepend(object[] array, params object[] args)
        {
            ISeq ret = ArraySeq.create(array);
            for (int i = args.Length - 1; i >= 0; --i)
                ret = RT.cons(args[i], ret);
            return ret;
        }

        #endregion

        #region Misc

        // do we need this?
        protected static ISeq FindKey(object key, ISeq args)
        {
            while (args != null)
            {
                if (key == args.first())
                    return args.next();
                args = RT.next(args);
                args = RT.next(args);
            }
            return null;
        }

        #endregion
    }
}
