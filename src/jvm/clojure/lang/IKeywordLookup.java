/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Oct 31, 2009 */

package clojure.lang;

public interface IKeywordLookup{
ILookupThunk getLookupThunk(Keyword k);

default IFn getLookupFn(Object k, IFn fault){
    ILookupThunk t = getLookupThunk((Keyword) k);
    if(t == null){
        Class c = this.getClass();
        return getNonFieldFn(fault, c);
    }

    return getFieldFn(fault, t);
}

private static AFn getFieldFn(IFn fault, ILookupThunk t){
    return new AFn(){
        public Object invoke(Object m, Object k){
            Object ret = t.get(m);
            if(ret == t)
                return fault.invoke(m, k);
            return ret;
        }
    };
}

private static AFn getNonFieldFn(IFn fault, Class c){
    return new AFn(){
        public Object invoke(Object m, Object k){
            if(m.getClass() == c)
                return RT.get(m, k);
            return fault.invoke(m, k);
        }
    };
}
}
