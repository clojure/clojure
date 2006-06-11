/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 3, 2006 */

package clojure.runtime;

import java.util.Iterator;

public class IteratorIter implements Iter{

Iterator i;
Object val;

IteratorIter(Iterator i)
	{
	if(!i.hasNext())
		throw new IllegalStateException("Iterator must have elements to construct Iter");
	this.i = i;
	val = i.next();
	}

public Object get()
	{
	return val;
	}

public Iter iterate()
	{
	if(i.hasNext())
		{
		val = i.next();
		return this;
		}
	return null;
	}
}
