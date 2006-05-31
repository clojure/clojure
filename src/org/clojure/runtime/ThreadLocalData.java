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

package org.clojure.runtime;

import java.util.IdentityHashMap;

public class ThreadLocalData{

final public static int MULTIPLE_VALUES_LIMIT = 20;
public int mvCount = 0;
public Object[] mvArray = new Object[MULTIPLE_VALUES_LIMIT];

IdentityHashMap dynamicBindings = new IdentityHashMap();
Transaction transaction;

public Transaction getTransaction() throws Exception{
	if(transaction == null)
		throw new Exception("No active transaction");
	return transaction;
}

public ThreadLocalData(IdentityHashMap dynamicBindings)
	{
	this.mvCount = 0;
	this.mvArray = new Object[MULTIPLE_VALUES_LIMIT];
	this.dynamicBindings = dynamicBindings;
	}

public ThreadLocalData()
	{
	this(new IdentityHashMap());
	}

public static ThreadLocalData get()
	{
	return (ThreadLocalData) tld.get();
	}

static InheritableThreadLocal tld = new InheritableThreadLocal(){
	protected Object childValue(Object object)
		{
		return new ThreadLocalData((IdentityHashMap) ((ThreadLocalData) object).dynamicBindings.clone());
		}

	protected Object initialValue()
		{
		return new ThreadLocalData();
		}
};

}
