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

package clojure.lang;

public class ThreadLocalData{

private static ThreadLocal<Object[]> values = new ThreadLocal<Object[]>();

static public Object[] getValues(){
	return values.get();
}

static public void setValues(Object[] vals){
	values.set(vals);
}


private static ThreadLocal<Integer> tltest = new ThreadLocal<Integer>();
private static Integer test;
private static volatile int sum;
private static volatile Object fred;

static public void main(String[] args){
	test = new Integer(17);
	tltest.set(new Integer(17));
//	PersistentArrayIdentityMap testmap = PersistentArrayIdentityMap.EMPTY;
//	testmap = (PersistentArrayIdentityMap) testmap.put(42, 42);
//	testmap = (PersistentArrayIdentityMap) testmap.put(Thread.currentThread(), 17);

	IPersistentMap testmap = PersistentArrayMap.EMPTY;
	testmap = testmap.assoc(Thread.currentThread(), 17);
//	testmap = testmap.put(42, 42);
//	testmap = testmap.put(43, 42);
//	testmap = testmap.put(44, 42);
//	testmap = testmap.put(45, 42);

	int n = Integer.parseInt(args[0]);

	long startTime = System.nanoTime();
	sum = 0;
	for(int i = 0; i < n; i++)
		{
		sum += test.intValue();
		}

	long estimatedTime = System.nanoTime() - startTime;
	System.out.println("sum = " + sum + ", time: " + estimatedTime / 1000000);

	startTime = System.nanoTime();
	sum = 0;
	for(int i = 0; i < n; i++)
		{
		sum += tltest.get().intValue();
		}

	estimatedTime = System.nanoTime() - startTime;
	System.out.println("sum = " + sum + ", time: " + estimatedTime / 1000000);

	startTime = System.nanoTime();
	sum = 0;
	for(int i = 0; i < n; i++)
		{
		sum += ((Integer) testmap.valAt(Thread.currentThread())).intValue();
		}

	estimatedTime = System.nanoTime() - startTime;
	System.out.println("sum = " + sum + ", time: " + estimatedTime / 1000000);

	startTime = System.nanoTime();
	sum = 0;
	for(int i = 0; i < n; i++)
		{
		if(fred != null)
			sum += ((Integer) testmap.valAt(Thread.currentThread())).intValue();
		else
			sum += test.intValue();
		}

	estimatedTime = System.nanoTime() - startTime;
	System.out.println("sum = " + sum + ", time: " + estimatedTime / 1000000);

}
}
