/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:01:29 AM */

using System;

namespace clojure.lang
{

public class Cons : ASeq
				  {

private readonly Object _first;
private readonly ISeq _rest;

public Cons(Object first, ISeq rest)
	{
	this._first = first;
	this._rest = rest;
	}


#region ISeq Members

override public object first()
    {
    return _first;
    }

override public ISeq rest()
    {
    return _rest;
    }

#endregion

	}

}
