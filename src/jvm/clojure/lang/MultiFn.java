/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Sep 13, 2007 */

package clojure.lang;

import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class MultiFn extends AFn{
final public IFn dispatchFn;
final public Object defaultDispatchVal;
final public IRef hierarchy;
final String name;
final ReentrantReadWriteLock rw;
volatile IPersistentMap methodTable;
volatile IPersistentMap preferTable;
volatile IPersistentMap methodCache;
volatile Object cachedHierarchy;

static final Var assoc = RT.var("clojure.core", "assoc");
static final Var dissoc = RT.var("clojure.core", "dissoc");
static final Var isa = RT.var("clojure.core", "isa?");
static final Var parents = RT.var("clojure.core", "parents");

public MultiFn(String name, IFn dispatchFn, Object defaultDispatchVal, IRef hierarchy) {
	this.rw = new ReentrantReadWriteLock();
	this.name = name;
	this.dispatchFn = dispatchFn;
	this.defaultDispatchVal = defaultDispatchVal;
	this.methodTable = PersistentHashMap.EMPTY;
	this.methodCache = getMethodTable();
	this.preferTable = PersistentHashMap.EMPTY;
    this.hierarchy = hierarchy;
	cachedHierarchy = null;
}

public MultiFn reset(){
	rw.writeLock().lock();
	try{
		methodTable = methodCache = preferTable = PersistentHashMap.EMPTY;
		cachedHierarchy = null;
		return this;
	}
	finally {
		rw.writeLock().unlock();
	}
}

public MultiFn addMethod(Object dispatchVal, IFn method) {
	rw.writeLock().lock();
	try{
		methodTable = getMethodTable().assoc(dispatchVal, method);
		resetCache();
		return this;
	}
	finally {
		rw.writeLock().unlock();
	}
}

public MultiFn removeMethod(Object dispatchVal) {
	rw.writeLock().lock();
	try
		{
		methodTable = getMethodTable().without(dispatchVal);
		resetCache();
		return this;
		}
	finally
		{
		rw.writeLock().unlock();
		}
}

public MultiFn preferMethod(Object dispatchValX, Object dispatchValY) {
	rw.writeLock().lock();
	try
		{
		if(prefers(dispatchValY, dispatchValX))
			throw new IllegalStateException(
					String.format("Preference conflict in multimethod '%s': %s is already preferred to %s",
					              name, dispatchValY, dispatchValX));
		preferTable = getPreferTable().assoc(dispatchValX, RT.conj((IPersistentCollection) RT.get(getPreferTable(),
		                                                                                     dispatchValX,
		                                                                                     PersistentHashSet.EMPTY),
		                                                      dispatchValY));
		resetCache();
		return this;
		}
	finally
		{
		rw.writeLock().unlock();
		}
}

private boolean prefers(Object x, Object y) {
	IPersistentSet xprefs = (IPersistentSet) getPreferTable().valAt(x);
	if(xprefs != null && xprefs.contains(y))
		return true;
	for(ISeq ps = RT.seq(parents.invoke(y)); ps != null; ps = ps.next())
		{
		if(prefers(x, ps.first()))
			return true;
		}
	for(ISeq ps = RT.seq(parents.invoke(x)); ps != null; ps = ps.next())
		{
		if(prefers(ps.first(), y))
			return true;
		}
	return false;
}

private boolean isA(Object x, Object y) {
    return RT.booleanCast(isa.invoke(hierarchy.deref(), x, y));
}

private boolean dominates(Object x, Object y) {
	return prefers(x, y) || isA(x, y);
}

private IPersistentMap resetCache() {
	rw.writeLock().lock();
	try
		{
		methodCache = getMethodTable();
		cachedHierarchy = hierarchy.deref();
		return methodCache;
		}
	finally
		{
		rw.writeLock().unlock();
		}
}

 public IFn getMethod(Object dispatchVal) {
	if(cachedHierarchy != hierarchy.deref())
		resetCache();
	IFn targetFn = (IFn) methodCache.valAt(dispatchVal);
	if(targetFn != null)
		return targetFn;
	 targetFn = findAndCacheBestMethod(dispatchVal);
	if(targetFn != null)
		return targetFn;
	targetFn = (IFn) getMethodTable().valAt(defaultDispatchVal);
	return targetFn;
}

private IFn getFn(Object dispatchVal) {
	IFn targetFn = getMethod(dispatchVal);
	if(targetFn == null)
		throw new IllegalArgumentException(String.format("No method in multimethod '%s' for dispatch value: %s",
		                                                 name, dispatchVal));
	return targetFn;
}

private IFn findAndCacheBestMethod(Object dispatchVal) {
	rw.readLock().lock();
	Map.Entry bestEntry;
	IPersistentMap mt = methodTable;
	IPersistentMap pt = preferTable;
	Object ch = cachedHierarchy;
	try
		{
		bestEntry = null;
		for(Object o : getMethodTable())
			{
			Map.Entry e = (Map.Entry) o;
			if(isA(dispatchVal, e.getKey()))
				{
				if(bestEntry == null || dominates(e.getKey(), bestEntry.getKey()))
					bestEntry = e;
				if(!dominates(bestEntry.getKey(), e.getKey()))
					throw new IllegalArgumentException(
							String.format(
									"Multiple methods in multimethod '%s' match dispatch value: %s -> %s and %s, and neither is preferred",
									name, dispatchVal, e.getKey(), bestEntry.getKey()));
				}
			}
		if(bestEntry == null)
			return null;
		}
	finally
		{
		rw.readLock().unlock();
		}


	//ensure basis has stayed stable throughout, else redo
	rw.writeLock().lock();
	try
		{
		if( mt == methodTable &&
		    pt == preferTable &&
		    ch == cachedHierarchy &&
			cachedHierarchy == hierarchy.deref())
			{
			//place in cache
			methodCache = methodCache.assoc(dispatchVal, bestEntry.getValue());
			return (IFn) bestEntry.getValue();
			}
		else
			{
			resetCache();
			return findAndCacheBestMethod(dispatchVal);
			}
		}
	finally
		{
		rw.writeLock().unlock();
		}
}

public Object invoke() {
	return getFn(dispatchFn.invoke()).invoke();
}

public Object invoke(Object arg1) {
	return getFn(dispatchFn.invoke(arg1)).invoke(Util.ret1(arg1,arg1=null));
}

public Object invoke(Object arg1, Object arg2) {
	return getFn(dispatchFn.invoke(arg1, arg2)).
                     invoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3)).
                     invoke(Util.ret1(arg1,arg1=null), Util.ret1(arg2,arg2=null), Util.ret1(arg3,arg3=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null),
                            Util.ret1(arg9,arg9=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null),
                            Util.ret1(arg9,arg9=null),
                            Util.ret1(arg10,arg10=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null),
                            Util.ret1(arg9,arg9=null),
                            Util.ret1(arg10,arg10=null),
                            Util.ret1(arg11,arg11=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null),
                            Util.ret1(arg9,arg9=null),
                            Util.ret1(arg10,arg10=null),
                            Util.ret1(arg11,arg11=null),
                            Util.ret1(arg12,arg12=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) {
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)).
                     invoke(Util.ret1(arg1,arg1=null),
                            Util.ret1(arg2,arg2=null),
                            Util.ret1(arg3,arg3=null),
                            Util.ret1(arg4,arg4=null),
                            Util.ret1(arg5,arg5=null),
                            Util.ret1(arg6,arg6=null),
                            Util.ret1(arg7,arg7=null),
                            Util.ret1(arg8,arg8=null),
                            Util.ret1(arg9,arg9=null),
                            Util.ret1(arg10,arg10=null),
                            Util.ret1(arg11,arg11=null),
                            Util.ret1(arg12,arg12=null),
                            Util.ret1(arg13,arg13=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) {
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) {
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) {
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null),
                               Util.ret1(arg17,arg17=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) {
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null),
                               Util.ret1(arg17,arg17=null),
                               Util.ret1(arg18,arg18=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null),
                               Util.ret1(arg17,arg17=null),
                               Util.ret1(arg18,arg18=null),
                               Util.ret1(arg19,arg19=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19, arg20)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null),
                               Util.ret1(arg17,arg17=null),
                               Util.ret1(arg18,arg18=null),
                               Util.ret1(arg19,arg19=null),
                               Util.ret1(arg20,arg20=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20, Object... args)
		{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19, arg20, args)).
                        invoke(Util.ret1(arg1,arg1=null),
                               Util.ret1(arg2,arg2=null),
                               Util.ret1(arg3,arg3=null),
                               Util.ret1(arg4,arg4=null),
                               Util.ret1(arg5,arg5=null),
                               Util.ret1(arg6,arg6=null),
                               Util.ret1(arg7,arg7=null),
                               Util.ret1(arg8,arg8=null),
                               Util.ret1(arg9,arg9=null),
                               Util.ret1(arg10,arg10=null),
                               Util.ret1(arg11,arg11=null),
                               Util.ret1(arg12,arg12=null),
                               Util.ret1(arg13,arg13=null),
                               Util.ret1(arg14,arg14=null),
                               Util.ret1(arg15,arg15=null),
                               Util.ret1(arg16,arg16=null),
                               Util.ret1(arg17,arg17=null),
                               Util.ret1(arg18,arg18=null),
                               Util.ret1(arg19,arg19=null),
                               Util.ret1(arg20,arg20=null),
                               args);
}

    public IPersistentMap getMethodTable() {
        return methodTable;
    }

    public IPersistentMap getPreferTable() {
        return preferTable;
    }
}
