/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 29, 2006 10:39:05 AM */

using System;

namespace org.clojure.runtime
{

public class Keyword : Symbol
					 {
/**
 * Used by Namespace.intern()
 *
 * @param name
 * @param ns
 */
internal Keyword(String name, Namespace ns):
	base(name, ns)

	{
	this.val = this;
	this.fn = this;
	}

override public Object getValue(ThreadLocalData tld)
	{
	return this;
	}

override public Object setValue(ThreadLocalData tld, Object val)
	{
	throw new InvalidOperationException("Cannot set the value of a keyword");
	}

override public String ToString()
	{
	return ":" + name;
	}
}
}