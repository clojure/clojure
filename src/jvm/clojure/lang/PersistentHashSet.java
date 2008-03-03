/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 3, 2008 */

package clojure.lang;

import java.util.List;
import java.util.Iterator;

public class PersistentHashSet extends APersistentSet{

static public final PersistentHashSet EMPTY = new PersistentHashSet(null, PersistentHashMap.EMPTY);

public static PersistentHashSet create(Object... init){
	PersistentHashSet ret = EMPTY;
	for(int i = 0; i < init.length; i++)
		{
		ret = (PersistentHashSet) ret.cons(init[i]);
		}
	return ret;
}

public static PersistentHashSet create(List init){
	PersistentHashSet ret = EMPTY;
	for(Iterator i = init.iterator(); i.hasNext();)
		{
		Object key = i.next();
		ret = (PersistentHashSet) ret.cons(key);
		}
	return ret;
}

static public PersistentHashSet create(ISeq items){
	PersistentHashSet ret = EMPTY;
	for(; items != null; items = items.rest())
		{
		ret = (PersistentHashSet) ret.cons(items.first());
		}
	return ret;
}

PersistentHashSet(IPersistentMap meta, IPersistentMap impl){
	super(meta, impl);
}

public IPersistentSet disjoin(Object key) throws Exception{
	if(contains(key))
		return new PersistentHashSet(meta(),impl.without(key));
	return this;
}

public IPersistentCollection cons(Object o){
	if(contains(o))
		return this;
	return new PersistentHashSet(meta(),impl.assoc(o,o));
}
}
