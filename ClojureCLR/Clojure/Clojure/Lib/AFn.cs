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
using System.Collections;

namespace clojure.lang
{
    /// <summary>
    /// Provides a basic implementation of <see cref="IFn">IFn</see> interface methods.
    /// </summary>
    [Serializable]
    public abstract class AFn : Obj, IFn
    {
        #region C-tors

        /// <summary>
        /// Initialize an <see cref="AFn">AFn</see> with the given metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        public AFn(IPersistentMap meta)
            : base(meta)
        {
        }

        /// <summary>
        /// Initialize an <see cref="AFn">AFn</see> with the null metadata.
        /// </summary>
        public AFn()
        {
        }

        #endregion

        #region IObj members

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        /// <remarks>Not implemented at this level.  
        /// This just introduces the virtual method definition 
        /// for concrete classes to override, or not.  
        /// (Some may want to leave it unimplemented.)</remarks>
        public override IObj withMeta(IPersistentMap meta)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region IFn Members

        public virtual object invoke()
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16, object arg17)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, 
            object arg18)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, 
            object arg18, object arg19)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, 
            object arg18, object arg19, object arg20)
        {
            throw WrongArityException();
        }

        public virtual object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, 
            object arg6, object arg7, object arg8, object arg9, object arg10, object arg11, 
            object arg12, object arg13, object arg14, object arg15, object arg16, object arg17, 
            object arg18, object arg19, object arg20, params object[] args)
        {
            throw WrongArityException();
        }



        public virtual object applyTo(ISeq arglist)
        {
            return ApplyToHelper(this, arglist);
        }



        public static object ApplyToHelper(IFn ifn, ISeq arglist)
        {
            switch (RT.BoundedLength(arglist, 20))
            {
                case 0:
                    return ifn.invoke();
                case 1:
                    return ifn.invoke(arglist.first());
                case 2:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                    );
                case 3:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                    );
                case 4:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                    );
                case 5:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                    );
                case 6:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                    );
                case 7:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                    );
                case 8:
                    return ifn.invoke(arglist.first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , (arglist = arglist.next()).first()
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
                            , RT.SeqToArray<object>(arglist.next()));
            }
        }


        public Exception WrongArityException()
        {
            string name = GetType().Name;
            int suffix = name.LastIndexOf("__");  // NOt sure if this is necessary
            return new ArgumentException(String.Format("Wrong number of args passed to: {0}",
                (suffix == -1 ? name : name.Substring(0, suffix)).Replace('_', '-')));
        }

        public Exception WrongArityException2()
        {
            Console.WriteLine("Do-dah!");
            return WrongArityException();
        }


        #endregion
    }
}
