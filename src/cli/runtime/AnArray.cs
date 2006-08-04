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
using System.Collections;

namespace clojure.lang
{
public abstract class AnArray : Obj, IArray {

	public override Obj withMeta(IPersistentMap meta)
		{
		Obj ret = (Obj)MemberwiseClone();
		ret._meta = meta;
		return ret;
		}


	#region IArray Members

	abstract public int length();

	abstract public object nth(int i);

	abstract public IArray assocN(int i, object val);

	#endregion

	#region IPersistentCollection Members

	abstract public int count();

	abstract public ISeq seq();

	#endregion
	}

}
