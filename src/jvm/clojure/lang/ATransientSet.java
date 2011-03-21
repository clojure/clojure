/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 3, 2008 */

package clojure.lang;

public abstract class ATransientSet extends AFn implements ITransientSet{
	ITransientMap impl;

	ATransientSet(ITransientMap impl) {
		this.impl = impl;
	}
	
	public int count() {
		return impl.count();
	}

	public ITransientSet conj(Object val) {
		ITransientMap m = impl.assoc(val, val);
		if (m != impl) this.impl = m;
		return this;
	}

	public boolean contains(Object key) {
		return this != impl.valAt(key, this);
	}

	public ITransientSet disjoin(Object key)  {
		ITransientMap m = impl.without(key);
		if (m != impl) this.impl = m;
		return this;
	}

	public Object get(Object key) {
		return impl.valAt(key);
	}

	public Object invoke(Object key, Object notFound)  {
		return impl.valAt(key, notFound);
	}

	public Object invoke(Object key)  {
		return impl.valAt(key);	
	}
	
}
