/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 16, 2007 */

package clojure.lang;

import java.util.Iterator;
import java.util.Map;

public class PersistentStructMap extends APersistentMap{

final IPersistentMap keyslots;
final Object[] vals;
final IPersistentMap ext;

static public IPersistentMap createSlotMap(ISeq keys){
	if(keys == null)
		throw new IllegalArgumentException("Must supply keys");
	PersistentHashMap temp = PersistentHashMap.EMPTY;
	for(ISeq s = keys; s != null; s = s.rest())
		{
		temp = (PersistentHashMap) temp.assoc(s.first(), null);
		}
	int i = 0;
	PersistentHashMap ret = PersistentHashMap.EMPTY;
	for(ISeq s = RT.keys(temp); s != null; s = s.rest(), i++)
		ret = (PersistentHashMap) ret.assoc(s.first(), i);
	return ret;
}

static public PersistentStructMap create(IPersistentMap keyslots, ISeq init){
	Object[] vals = new Object[keyslots.count()];
	IPersistentMap ext = PersistentHashMap.EMPTY;
	for(; init != null; init = init.rest().rest())
		{
		if(init.rest() == null)
			throw new IllegalArgumentException(String.format("No value supplied for key: %s", init.first()));
		Object k = init.first();
		Object v = RT.second(init);
		Map.Entry e = keyslots.entryAt(k);
		if(e != null)
			vals[(Integer) e.getValue()] = v;
		else
			ext = ext.assoc(k, v);
		}
	return new PersistentStructMap(null, keyslots, vals, ext);
}

static public IFn getAccessor(final IPersistentMap keyslots, Object key){
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		{
		final int i = (Integer) e.getValue();
		return new AFn(){
			public Object invoke(Object arg1) throws Exception{
				PersistentStructMap m = (PersistentStructMap) arg1;
				if(m.keyslots != keyslots)
					throw new Exception("Accessor/struct mismatch");
				return m.vals[i];
			}
		};
		}
	throw new IllegalArgumentException("Not a key of struct");
}

PersistentStructMap(IPersistentMap meta, IPersistentMap keys, Object[] vals, IPersistentMap ext){
	super(meta);
	this.ext = ext;
	this.keyslots = keys;
	this.vals = vals;
}

public boolean containsKey(Object key){
	return keyslots.containsKey(key) || ext.containsKey(key);
}

public IMapEntry entryAt(Object key){
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		{
		return new MapEntry(key, vals[(Integer) e.getValue()]);
		}
	return ext.entryAt(key);
}

public IPersistentMap assoc(Object key, Object val){
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		{
		int i = (Integer) e.getValue();
		Object[] newVals = vals.clone();
		newVals[i] = val;
		return new PersistentStructMap(_meta, keyslots, newVals, ext);
		}
	return new PersistentStructMap(_meta, keyslots, vals, ext.assoc(key, val));
}

public Object valAt(Object key){
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		{
		return vals[(Integer) e.getValue()];
		}
	return ext.valAt(key);
}

public Object valAt(Object key, Object notFound){
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		{
		return vals[(Integer) e.getValue()];
		}
	return ext.valAt(key, notFound);
}

public IPersistentMap assocEx(Object key, Object val) throws Exception{
	if(containsKey(key))
		throw new Exception("Key already present");
	return assoc(key, val);
}

public IPersistentMap without(Object key) throws Exception{
	Map.Entry e = keyslots.entryAt(key);
	if(e != null)
		throw new Exception("Can't remove struct key");
	IPersistentMap newExt = ext.without(key);
	if(newExt == ext)
		return this;
	return new PersistentStructMap(_meta, keyslots, vals, newExt);
}

public Iterator iterator(){
	return new SeqIterator(seq());
}


public int count(){
	return vals.length + RT.count(ext);
}

public ISeq seq(){
	return new Seq(null, RT.keys(keyslots), vals, 0, ext);
}

static class Seq extends ASeq{
	final int i;
	final ISeq keys;
	final Object[] vals;
	final IPersistentMap ext;


	public Seq(IPersistentMap meta, ISeq keys, Object[] vals, int i, IPersistentMap ext){
		super(meta);
		this.i = i;
		this.keys = keys;
		this.vals = vals;
		this.ext = ext;
	}

	public Obj withMeta(IPersistentMap meta){
		if(meta != _meta)
			return new Seq(meta, keys, vals, i, ext);
		return this;
	}

	public Object first(){
		return new MapEntry(keys.first(), vals[i]);
	}

	public ISeq rest(){
		if(i + 1 < vals.length)
			return new Seq(_meta, keys.rest(), vals, i + 1, ext);
		return ext.seq();
	}
}
}
