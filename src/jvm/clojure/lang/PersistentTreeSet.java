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
import java.util.Comparator;

public class PersistentTreeSet extends APersistentSet implements Reversible, Sorted{
static public final PersistentTreeSet EMPTY = new PersistentTreeSet(null, PersistentTreeMap.EMPTY);

public static PersistentTreeSet create(Object... init){
	PersistentTreeSet ret = EMPTY;
	for(int i = 0; i < init.length; i++)
		{
		ret = (PersistentTreeSet) ret.cons(init[i]);
		}
	return ret;
}

public static PersistentTreeSet create(Comparator comp, Object... init){
	PersistentTreeSet ret = new PersistentTreeSet(null, new PersistentTreeMap(null, comp));
	for(int i = 0; i < init.length; i++)
		{
		ret = (PersistentTreeSet) ret.cons(init[i]);
		}
	return ret;
}

public static PersistentTreeSet create(List init){
	PersistentTreeSet ret = EMPTY;
	for(Object key : init)
		{
		ret = (PersistentTreeSet) ret.cons(key);
		}
	return ret;
}

public static PersistentTreeSet create(Comparator comp, List init){
	PersistentTreeSet ret = new PersistentTreeSet(null, new PersistentTreeMap(null, comp));
	for(Object key : init)
		{
		ret = (PersistentTreeSet) ret.cons(key);
		}
	return ret;
}

static public PersistentTreeSet create(ISeq items){
	PersistentTreeSet ret = EMPTY;
	for(; items != null; items = items.next())
		{
		ret = (PersistentTreeSet) ret.cons(items.first());
		}
	return ret;
}

static public PersistentTreeSet create(Comparator comp, ISeq items){
	PersistentTreeSet ret = new PersistentTreeSet(null, new PersistentTreeMap(null, comp));
	for(; items != null; items = items.next())
		{
		ret = (PersistentTreeSet) ret.cons(items.first());
		}
	return ret;
}

PersistentTreeSet(IPersistentMap meta, IPersistentMap impl){
	super(meta, impl);
}

public IPersistentSet disjoin(Object key) throws Exception{
	if(contains(key))
		return new PersistentTreeSet(meta(),impl.without(key));
	return this;
}

public IPersistentSet cons(Object o){
	if(contains(o))
		return this;
	return new PersistentTreeSet(meta(),impl.assoc(o,o));
}

public IPersistentCollection empty(){
	return EMPTY.withMeta(meta());	
}

public ISeq rseq() throws Exception{
	return APersistentMap.KeySeq.create(((Reversible) impl).rseq());
}

public PersistentTreeSet withMeta(IPersistentMap meta){
	return new PersistentTreeSet(meta, impl);
}

public Comparator comparator(){
	return ((Sorted)impl).comparator();
}

public Object entryKey(Object entry){
	return entry;
}

public ISeq seq(boolean ascending){
	PersistentTreeMap m = (PersistentTreeMap) impl;
	return RT.keys(m.seq(ascending));
}

public ISeq seqFrom(Object key, boolean ascending){
	PersistentTreeMap m = (PersistentTreeMap) impl;
	return RT.keys(m.seqFrom(key,ascending));
}

}
