/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Nov 2, 2009 */

package clojure.lang;

public final class KeywordLookupSite implements ILookupSite, ILookupThunk{

final int n;
final Keyword k;

public KeywordLookupSite(int n, Keyword k){
	this.n = n;
	this.k = k;
}

public Object fault(Object target, ILookupHost host){
	if(target instanceof IKeywordLookup)
		{
		return install(target, host);
		}
	else if(target instanceof ILookup)
		{
		host.swapThunk(n,ilookupThunk(target.getClass()));
		return ((ILookup) target).valAt(k);
		}
	host.swapThunk(n,this);
	return RT.get(target, k);
}

public Object get(Object target){
	if(target instanceof IKeywordLookup || target instanceof ILookup)
		return this;
	return RT.get(target,k);
}

private ILookupThunk ilookupThunk(final Class c){
	return new ILookupThunk(){
			public Object get(Object target){
				if(target != null && target.getClass() == c)
					return ((ILookup) target).valAt(k);
				return this;
			}
		};
}

private Object install(Object target, ILookupHost host){
	ILookupThunk t = ((IKeywordLookup)target).getLookupThunk(k);
	if(t != null)
		{
		host.swapThunk(n,t);
		return t.get(target);
		}
	host.swapThunk(n,ilookupThunk(target.getClass()));
	return ((ILookup) target).valAt(k);
}
}
