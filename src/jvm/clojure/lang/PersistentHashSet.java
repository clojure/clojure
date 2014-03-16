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

import java.util.List;

public class PersistentHashSet extends APersistentSet implements IObj, IEditableCollection {

static public final PersistentHashSet EMPTY = new PersistentHashSet(null, PersistentHashMap.EMPTY);

final IPersistentMap _meta;

public static PersistentHashSet create(Object... init){
	ITransientSet ret = (ITransientSet)EMPTY.asTransient();
	for(int i = 0; i < init.length; i++)
		{
		ret = (ITransientSet)ret.conj(init[i]);
		}
	return (PersistentHashSet)ret.persistent();
}

public static PersistentHashSet create(List init){
	ITransientSet ret = (ITransientSet)EMPTY.asTransient();
	for(Object key : init)
		{
		ret = (ITransientSet) ret.conj(key);
		}
	return (PersistentHashSet)ret.persistent();
}

static public PersistentHashSet create(ISeq items){
	ITransientSet ret = (ITransientSet)EMPTY.asTransient();
	for(; items != null; items = items.next())
		{
		ret = (ITransientSet) ret.conj(items.first());
		}
	return (PersistentHashSet)ret.persistent();
}

public static PersistentHashSet createWithCheck(Object... init){
	PersistentHashSet ret = EMPTY;
	for(int i = 0; i < init.length; i++)
		{
		ret = (PersistentHashSet) ret.cons(init[i]);
		if(ret.count() != i + 1)
			throw new IllegalArgumentException("Duplicate key: " + init[i]);
		}
	return ret;
}

public static PersistentHashSet createWithCheck(List init){
	PersistentHashSet ret = EMPTY;
	int i=0;
	for(Object key : init)
		{
		ret = (PersistentHashSet) ret.cons(key);
		if(ret.count() != i + 1)
			throw new IllegalArgumentException("Duplicate key: " + key);		
		++i;
		}
	return ret;
}

static public PersistentHashSet createWithCheck(ISeq items){
	PersistentHashSet ret = EMPTY;
	for(int i=0; items != null; items = items.next(), ++i)
		{
		ret = (PersistentHashSet) ret.cons(items.first());
		if(ret.count() != i + 1)
			throw new IllegalArgumentException("Duplicate key: " + items.first());
		}
	return ret;
}

PersistentHashSet(IPersistentMap meta, IPersistentMap impl){
	super(impl);
	this._meta = meta;
}

public IPersistentSet disjoin(Object key) {
	if(contains(key))
		return new PersistentHashSet(meta(),impl.without(key));
	return this;
}

public IPersistentSet cons(Object o){
	if(contains(o))
		return this;
	return new PersistentHashSet(meta(),impl.assoc(o,o));
}

public IPersistentCollection empty(){
	return EMPTY.withMeta(meta());	
}

public PersistentHashSet withMeta(IPersistentMap meta){
	return new PersistentHashSet(meta, impl);
}

public ITransientCollection asTransient() {
	return new TransientHashSet(((PersistentHashMap) impl).asTransient());
}

public IPersistentMap meta(){
	return _meta;
}

static final class TransientHashSet extends ATransientSet {
	TransientHashSet(ITransientMap impl) {
		super(impl);
	}

	public IPersistentCollection persistent() {
		return new PersistentHashSet(null, impl.persistent());
	}
}

}
