/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:45:22 AM */

using System;
using System.Collections.Specialized;

namespace org.clojure.runtime
{
public class ThreadLocalData{

public const int MULTIPLE_VALUES_LIMIT = 20;
public int mvCount = 0;
public Object[] mvArray = new Object[MULTIPLE_VALUES_LIMIT];

internal HybridDictionary dynamicBindings = new HybridDictionary();

public ThreadLocalData(HybridDictionary dynamicBindings)
	{
	this.mvCount = 0;
	this.mvArray = new Object[MULTIPLE_VALUES_LIMIT];
	this.dynamicBindings = dynamicBindings;
	}

public ThreadLocalData():
	this(new HybridDictionary())
	{
	}

public static ThreadLocalData get()
	{
	if(tld == null)
		tld = new ThreadLocalData();
	return tld;
	}

/*
note this is not the same semantics as InheritableThreadLocal - aargh
might need to make Java side non-inheritable
*/
[ThreadStatic]
	static ThreadLocalData tld;
	/* was this in Java
static InheritableThreadLocal tld = new InheritableThreadLocal(){
	protected Object childValue(Object object)
		{
		return new ThreadLocalData((HybridDictionary) ((ThreadLocalData) object).dynamicBindings.clone());
		}

	protected Object initialValue()
		{
		return new ThreadLocalData();
		}
};
*/

}
}
