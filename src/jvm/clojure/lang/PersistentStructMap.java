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

public static class Def{
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

static public Def createSlotMap(ISeq keys){
	if(keys == null)
		throw new IllegalArgumentException("Must supply keys");
	PersistentHashMap ret = PersistentHashMap.EMPTY;
	int i=0;
	for(ISeq s = keys; s != null; s = s.rest(),i++)
		{
		ret = (PersistentHashMap) ret.assoc(s.first(), i);
		}
	return new Def(keys,ret);
}

static public PersistentStructMap create(Def def, ISeq keyvals){
	Object[] vals = new Object[def.keyslots.count()];
	IPersistentMap ext = PersistentHashMap.EMPTY;
	for(; keyvals != null; keyvals = keyvals.rest().rest())
		{
		if(keyvals.rest() == null)
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
	for(int i=0;i<vals.length && valseq != null; valseq = valseq.rest(),i++)
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
			public Object invoke(Object arg1) throws Exception{
				PersistentStructMap m = (PersistentStructMap) arg1;
				if(m.def != def)
					throw new Exception("Accessor/struct mismatch");
				return m.vals[i];
			}
		};
		}
	throw new IllegalArgumentException("Not a key of struct");
}

PersistentStructMap(IPersistentMap meta, Def def, Object[] vals, IPersistentMap ext){
	super(meta);
	this.ext = ext;
	this.def = def;
	this.vals = vals;
}


public Obj withMeta(IPersistentMap meta){
	if(meta == _meta)
		return this;
	return new PersistentStructMap(meta, def, vals, ext);
}

public boolean containsKey(Object key){
	return def.keyslots.containsKey(key) || ext.containsKey(key);
}

public IMapEntry entryAt(Object key){
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		{
		return new MapEntry(key, vals[(Integer) e.getValue()]);
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
		return new PersistentStructMap(_meta, def, newVals, ext);
		}
	return new PersistentStructMap(_meta, def, vals, ext.assoc(key, val));
}

public Object valAt(Object key){
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		{
		return vals[(Integer) e.getValue()];
		}
	return ext.valAt(key);
}

public Object valAt(Object key, Object notFound){
	Map.Entry e = def.keyslots.entryAt(key);
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
	Map.Entry e = def.keyslots.entryAt(key);
	if(e != null)
		throw new Exception("Can't remove struct key");
	IPersistentMap newExt = ext.without(key);
	if(newExt == ext)
		return this;
	return new PersistentStructMap(_meta, def, vals, newExt);
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
