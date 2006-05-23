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

package org.clojure.runtime;

public class Cons extends AMap implements Iter{

public final Object first;
public final Cons rest;

public Cons(Object first, Cons rest)
	{
	this.first = first;
	this.rest = rest;
	}

public Object get()
	{
	return first;
	}

public Iter iterate()
	{
	return rest;
	}

public Iter iter()
	{
	return this;
	}
}
