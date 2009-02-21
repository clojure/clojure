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
    /// Faking a few of the methods from the Java ConcurrentHashTable class.
    /// </summary>
    public class JavaConcurrentDictionary<TKey, TValue>
    {
        Dictionary<TKey, TValue> _dict = new Dictionary<TKey, TValue>();

        public TValue Get(TKey key)
        {
            lock (_dict)
            {
                TValue val;
                return _dict.TryGetValue(key, out val) ? val : default(TValue);
            }
        }

        public TValue PutIfAbsent(TKey key, TValue val)
        {
            lock (_dict)
            {
                TValue existingVal;
                if (_dict.TryGetValue(key, out existingVal))
                    return existingVal;
                else
                {
                    _dict[key] = val;
                    return default(TValue);
                }
            }
        }

        public TValue Remove(TKey key)
        {
            lock ( _dict )
            {
                TValue existingVal;
                if ( _dict.TryGetValue(key,out existingVal) )
                {
                    _dict.Remove(key);
                    return existingVal;
                }
                else
                    return default(TValue);
            }
        }

        public TValue[] Values
        {
            get
            {
                lock (_dict)
                {
                    Dictionary<TKey, TValue>.ValueCollection coll = _dict.Values;
                    TValue[] values = new TValue[coll.Count];
                    coll.CopyTo(values, 0);
                    return values;
                }
            }
        }

    }
}
