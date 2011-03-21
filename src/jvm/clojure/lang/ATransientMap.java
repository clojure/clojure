/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.util.Map;

import clojure.lang.PersistentHashMap.INode;

abstract class ATransientMap extends AFn implements ITransientMap {
	abstract void ensureEditable();
	abstract ITransientMap doAssoc(Object key, Object val);
	abstract ITransientMap doWithout(Object key);
	abstract Object doValAt(Object key, Object notFound);
	abstract int doCount();
	abstract IPersistentMap doPersistent();

	public ITransientMap conj(Object o) {
		ensureEditable();
		if(o instanceof Map.Entry)
			{
			Map.Entry e = (Map.Entry) o;
		
			return assoc(e.getKey(), e.getValue());
			}
		else if(o instanceof IPersistentVector)
			{
			IPersistentVector v = (IPersistentVector) o;
			if(v.count() != 2)
				throw new IllegalArgumentException("Vector arg to map conj must be a pair");
			return assoc(v.nth(0), v.nth(1));
			}
		
		ITransientMap ret = this;
		for(ISeq es = RT.seq(o); es != null; es = es.next())
			{
			Map.Entry e = (Map.Entry) es.first();
			ret = ret.assoc(e.getKey(), e.getValue());
			}
		return ret;
	}

	public final Object invoke(Object arg1) {
		return valAt(arg1);
	}

	public final Object invoke(Object arg1, Object notFound) {
		return valAt(arg1, notFound);
	}

	public final Object valAt(Object key) {
		return valAt(key, null);
	}

	public final ITransientMap assoc(Object key, Object val) {
		ensureEditable();
		return doAssoc(key, val);
	}

	public final ITransientMap without(Object key) {
		ensureEditable();
		return doWithout(key);
	}

	public final IPersistentMap persistent() {
		ensureEditable();
		return doPersistent();
	}

	public final Object valAt(Object key, Object notFound) {
		ensureEditable();
		return doValAt(key, notFound);
	}

	public final int count() {
		ensureEditable();
		return doCount();
	}
}
