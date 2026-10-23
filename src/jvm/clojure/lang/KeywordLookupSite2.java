/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/

/* rich 10/22/26 */

package clojure.lang;

public class KeywordLookupSite2 {
final Keyword k;
private ILookupThunk cache;

public KeywordLookupSite2(Keyword k){
    this.k = k;
    this.cache = new ILookupThunk(){
        @Override
        public Object get(Object target){
            return this;
        }
    };
}

public final ILookupThunk thunk(){
    return cache;
}

public final Object fault(IKeywordLookup target){
    ILookupThunk t = target.getLookupThunk(k);
    if(t != null){
        cache = t;
        return t.get(target);
    }
    return RT.get(target,k);
}

}
