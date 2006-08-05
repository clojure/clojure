/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Threading;
using System.Collections;

namespace clojure.lang
{
public abstract class APersistentMap : Obj, IPersistentMap{

	public override Obj withMeta(IPersistentMap meta)
		{
		if(_meta == meta)
			return this;
		Obj ret = (Obj)MemberwiseClone();
		ret._meta = meta;
		return ret;
		}


	#region IPersistentMap Members

	abstract public IPersistentMap add(object key, object val);


	abstract public IPersistentMap remove(object key);



	#endregion

	#region Associative Members

	abstract public bool contains(object key);

	abstract public IMapEntry find(object key);

	abstract public object get(object key);

	abstract public IPersistentMap assoc(object key, object val);

	#endregion

	#region IEnumerable Members

	abstract public IEnumerator GetEnumerator();

	#endregion

	#region IPersistentCollection Members

	abstract public int count();

	abstract public ISeq seq();

	#endregion
	}

}
