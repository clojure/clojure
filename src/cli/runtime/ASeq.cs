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

namespace clojure.lang
	{
	
public abstract class ASeq : Obj, ISeq{
	
	public override Obj withMeta(IPersistentMap meta)
		{
		if(_meta == meta)
			return this;
		Obj ret = (Obj)MemberwiseClone();
		ret._meta = meta;
		return ret;
		}
		
public virtual Object peek() {
    return first();
}

public virtual IPersistentList pop() {
    return rest();
}

public virtual int count() {
    return 1 + RT.count(rest());
}

public virtual ISeq seq() {
    return this;
}

public virtual IPersistentCollection cons(Object o) {
    return new Cons(o, this);
}

#region ISeq Members

abstract public object first();

abstract public ISeq rest();

#endregion
	}

}
