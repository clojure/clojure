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
    public class ArraySeq : ASeq, IndexedSeq, IReduce
    {
        // TODO: rethink the whole thing.
        // Java version has separate types for numeric arrays.
        // We could specialize for arrays using direct access versus arrays using Array.get access.

        #region Data

        private readonly object _a;
        private readonly int _i;
        private readonly object[] _oa;
        // TODO: Check against current version.  IList equivalent not in current Java version.
        private readonly IList _ilist;

        public object[] ToArray()
        {
            if ( _oa != null )
                return _oa;

            if (_ilist != null)
            {
                object[] items = new object[_ilist.Count];
                for (int i = 0; i < _ilist.Count; i++)
                    items[i] = _ilist[i];
                return items;
            }

            return (object[])_a;
        }

        #endregion

        #region C-tors and factory methods

        static public ArraySeq create()
        {
            return null;
        }

        static public ArraySeq create(params object[] array)
        {
            return (array == null || array.Length == 0)
                ? null
                : new ArraySeq(array, 0);
        }

        // Not in the Java version, but I can really use this
        static public ArraySeq create(object[] array, int firstIndex)
        {
            return (array == null || array.Length <= firstIndex )
                ? null
                : new ArraySeq(array, firstIndex);
        }

        internal static ISeq createFromObject(Object array)
        {
            if (array == null || ((Array)array).Length == 0)
                return null;
            //Type aclass = array.GetType();
            //if(aclass == int[].class)
            //    return new ArraySeq_int(null, (int[]) array, 0);
            //if(aclass == float[].class)
            //    return new ArraySeq_float(null, (float[]) array, 0);
            //if(aclass == double[].class)
            //    return new ArraySeq_double(null, (double[]) array, 0);
            //if(aclass == long[].class)
            //    return new ArraySeq_long(null, (long[]) array, 0);
            return new ArraySeq(array, 0);
        }


//  TODO:  Really need to think about this.  A lot left on the table here.

        ArraySeq(object array, int i)
        {
            _a = array;
            _i = i;
            _oa = (object[])(array is object[] ? array : null);
            _ilist = (IList)_a;
        }

        ArraySeq(IPersistentMap meta, object array, int i)
            : base(meta)
        {
            _a = array;
            _i = i;
            _oa = (object[])(array is object[] ? array : null);
            _ilist = (IList)_a;
        }


        #endregion

        #region ISeq members

        public override object first()
        {
            if (_oa != null)
                return _oa[_i];
            else
                return _ilist[_i];  //rev 1112 wraps this in RT.prepRet, don't know why
        }

        public override ISeq rest()
        {
            if (_oa != null)
            {
                if (_i + 1 < _oa.Length)
                    return new ArraySeq(_a, _i + 1);
            }
            else
            {
                if (_i + 1 < _ilist.Count)
                    return new ArraySeq(_a, _i + 1);
            }
            return null;
        }

        #endregion

        #region IPersistentCollection members

        public override int count()
        {
            return _oa != null
                ? _oa.Length - _i
                : _ilist.Count - _i;
        }

        #endregion

        #region IObj members

        public override IObj withMeta(IPersistentMap meta)
        {
            // Java version does not do identity test
            return meta == _meta
                ? this
                : new ArraySeq(meta, _a, _i);
        }

        #endregion

        #region IndexedSeq Members

        public int index()
        {
            return _i;
        }

        #endregion

        #region IReduce Members

        public object reduce(IFn f)
        {
            if (_oa != null)
            {
                object ret = _oa[_i];
                for (int x = _i + 1; x < _oa.Length; x++)
                    ret = f.invoke(ret, _oa[x]);
                return ret;
            }
            object ret1 = _ilist[_i];    // JAVA 1112 wraps in RT.prepRet
            for (int x = _i + 1; x < _ilist.Count; x++)
                ret1 = f.invoke(ret1, _ilist[x]);       // JAVA 1112 wraps in RT.prepRet
            return ret1;
        }

        public object reduce(IFn f, object start)
        {
            if (_oa != null)
            {
                object ret = f.invoke(start, _oa[_i]);
                for (int x = _i + 1; x < _oa.Length; x++)
                    ret = f.invoke(ret, _oa[x]);
                return ret;
            }
            object ret1 = f.invoke(start, _ilist[_i]);    // JAVA 1112 wraps in RT.prepRet
            for (int x = _i + 1; x < _ilist.Count; x++)
                ret1 = f.invoke(ret1, _ilist[x]);         // JAVA 1112 wraps in RT.prepRet
            return ret1;
        }

        #endregion
    }
}
