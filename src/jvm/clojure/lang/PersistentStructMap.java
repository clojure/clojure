/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 16, 2007 */

package clojure.lang;

import java.util.Iterator;
import java.util.Map;
import java.io.Serializable;

public class PersistentStructMap extends APersistentMap implements IObj{

public static class Def implements Serializable{
	final ISeq keys;
	final IPersistentMap keyslots;

	Def(ISeq keys, IPersistentMap keyslots){
		this.keys = keys;
		this.keyslots = keyslots;
	}
}

final Def def;
final Object[] vals;
final IPersistentMap ext;
final IPersistentMap _meta;


static public Def createSlotMap(ISeq keys){
	if(keys == null)
		throw new IllegalArgumentException("Must supply keys");
	int c = RT.count(keys);
	Object[] v = new Object[2*c];
	int i = 0;
	for(ISeq s = keys; s != null; s = s.next(), i++)
		{
		v[2*i] =  s.first();
		v[2*i+1] = i;
		}
	return new Def(keys, RT.map(v));
}

static public PersistentStructMap create(Def def, ISeq keyvals){
	Object[] vals = new Object[def.keyslots.count()];
	IPersistentMap ext = PersistentHashMap.EMPTY;
	for(; keyvals != null; keyvals = keyvals.next().next())
		{
		if(keyvals.next() == null)
			throw new IllegalArgumentException(String.format("No value supplied for key: %s", keyvals.first()));
		Object k = keyvals.first();
		Object v = RT.second(keyvals);
		Map.Entry e = def.keyslots.entryAt(k);
		if(e != null)
			vals[(Integer) e.getValue()] = v;
		else
			ext = ext.assoc(k, v);
		}
	return new PersistentStructMap(null, def, vals, ext);
}

static public PersistentStructMap construct(Def def, ISeq valseq){
	Object[] vals = new Object[def.keyslots.count()];
	IPersistentMap ext = PersistentHashMap.EMPTY;
	for(int i = 0; i < vals.length && valseq != null; valseq = valseq.next(), i++)
		{
		vals[i] = valseq.first();
		}
	if(valseq != null)
		throw new IllegalArgumentException("Too many arguments to struct constructor");
	return new PersistentStructMap(null, def, vals, ext);
}

static public IFn getAccessor(final Def def, Object key){
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		{
		final int i = (Integer) e.getValue();
		return new AFn(){
			public Object invoke(Object arg1) {
				PersistentStructMap m = (PersistentStructMap) arg1;
				if(m.def != def)
					throw Util.runtimeException("Accessor/struct mismatch");
				return m.vals[i];
			}
		};
		}
	throw new IllegalArgumentException("Not a key of struct");
}

protected PersistentStructMap(IPersistentMap meta, Def def, Object[] vals, IPersistentMap ext){
	this._meta = meta;
	this.ext = ext;
	this.def = def;
	this.vals = vals;
}

/**
 * Returns a new instance of PersistentStructMap using the given parameters.
 * This function is used instead of the PersistentStructMap constructor by
 * all methods that return a new PersistentStructMap.  This is done so as to
 * allow subclasses to return instances of their class from all
 * PersistentStructMap methods.
 */
protected PersistentStructMap makeNew(IPersistentMap meta, Def def, Object[] vals, IPersistentMap ext){
	return new PersistentStructMap(meta, def, vals, ext);
}

public IObj withMeta(IPersistentMap meta){
	if(meta == _meta)
		return this;
	return makeNew(meta, def, vals, ext);
}

public IPersistentMap meta(){
	return _meta;
}

public boolean containsKey(Object key){
	return def.keyslots.containsKey(key) || ext.containsKey(key);
}

public IMapEntry entryAt(Object key){
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		{
		return new MapEntry(e.getKey(), vals[(Integer) e.getValue()]);
		}
	return ext.entryAt(key);
}

public IPersistentMap assoc(Object key, Object val){
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		{
		int i = (Integer) e.getValue();
		Object[] newVals = vals.clone();
		newVals[i] = val;
		return makeNew(_meta, def, newVals, ext);
		}
	return makeNew(_meta, def, vals, ext.assoc(key, val));
}

public Object valAt(Object key){
	Integer i = (Integer) def.keyslots.valAt(key);
	if(i != null)
		{
		return vals[i];
		}
	return ext.valAt(key);
}

public Object valAt(Object key, Object notFound){
	Integer i = (Integer) def.keyslots.valAt(key);
	if(i != null)
		{
		return vals[i];
		}
	return ext.valAt(key, notFound);
}

public IPersistentMap assocEx(Object key, Object val) {
	if(containsKey(key))
		throw Util.runtimeException("Key already present");
	return assoc(key, val);
}

public IPersistentMap without(Object key) {
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		throw Util.runtimeException("Can't remove struct key");
	IPersistentMap newExt = ext.without(key);
	if(newExt == ext)
		return this;
	return makeNew(_meta, def, vals, newExt);
}

public Iterator iterator(){
	return new SeqIterator(seq());
}


public int count(){
	return vals.length + RT.count(ext);
}

public ISeq seq(){
	return new Seq(null, def.keys, vals, 0, ext);
}

public IPersistentCollection empty(){
	return construct(def, null);
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

	public ISeq next(){
		if(i + 1 < vals.length)
			return new Seq(_meta, keys.next(), vals, i + 1, ext);
		return ext.seq();
	}
}
}
