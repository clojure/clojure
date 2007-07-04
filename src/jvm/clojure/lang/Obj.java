/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 3:44:58 PM */

package clojure.lang;

public class Obj implements Cloneable{

private IPersistentMap _meta;

public Obj(IPersistentMap meta){
	this._meta = meta;
}

public Obj(){
	_meta = null;
}

final public IPersistentMap meta(){
	return _meta;
}

final public Obj withMeta(IPersistentMap meta){
	if(_meta == meta)
		return this;
	try
		{
		Obj ret = (Obj) clone();
		ret._meta = meta;
		return ret;
		}
	catch(CloneNotSupportedException ignore)
		{
		return null;
		}
}
}
