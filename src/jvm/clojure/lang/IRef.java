/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Nov 17, 2007 */

package clojure.lang;

import java.util.Queue;
import java.util.LinkedList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class IRef implements Ref{
volatile Object state;
final Queue q = new LinkedList();
boolean busy = false;
boolean altering = false;

volatile ISeq errors = null;
//todo - make tuneable
final static Executor executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors());
//final static Executor executor = Executors.newCachedThreadPool();
final static ThreadLocal<PersistentVector> nested = new ThreadLocal<PersistentVector>();
final static ThreadLocal inAlter = new ThreadLocal();

static class Action implements Runnable{
	final IRef iref;
	final IFn fn;
	final ISeq args;


	public Action(IRef iref, IFn fn, ISeq args){
		this.iref = iref;
		this.args = args;
		this.fn = fn;
	}

	public void run(){
		nested.set(PersistentVector.EMPTY);
		boolean hadError = false;
		try
			{
			iref.doAlter(fn, args);
			}
		catch(Exception e)
			{
			//todo report/callback
			iref.errors = RT.cons(e, iref.errors);
			hadError = true;
			}

		if(!hadError)
			{
			for(ISeq s = nested.get().seq(); s != null; s = s.rest())
				{
				Action a = (Action) s.first();
				a.iref.enqueue(a);
				}
			}

		synchronized(iref)
			{
			if(!iref.q.isEmpty())
				{
				executor.execute((Runnable) iref.q.remove());
				}
			else
				{
				iref.busy = false;
				}
			}

		nested.set(null);
	}
}

public IRef(Object state){
	setState(state);
}

void setState(Object newState){
	if(newState instanceof IObj)
		{
		IObj o = (IObj) newState;
		if(RT.get(o.meta(), RT.IREF_KEY) != this)
			{
			newState = o.withMeta((IPersistentMap) RT.assoc(o.meta(), RT.IREF_KEY, this));
			}
		}
	state = newState;
}

public Object get(){
	return state;
}

public ISeq getErrors(){
	return errors;
}

public void clearErrors(){
	errors = null;
}

synchronized void doAlter(IFn fn, ISeq args) throws Exception{
	try
		{
		altering = true;
		setState(fn.applyTo(RT.cons(state, args)));
		}
	finally
		{
		altering = false;
		}
}

public Object alter(IFn fn, ISeq args) throws Exception{
	if(errors != null)
		{
		throw new Exception("IRef has errors", (Exception) RT.first(errors));
		}
	//Action action = new Action(this, fn, args);
	if(altering)
		throw new Exception("Recursive change");
	LockingTransaction trans = LockingTransaction.getRunning();
	if(trans != null)
		throw new Exception("Cannot alter an IRef in a transaction");
	if(inAlter.get() != null)
		throw new Exception("Cannot nest alters, use commute");

	try
		{
		inAlter.set(this);
		doAlter(fn, args);
		}
	finally
		{
		inAlter.set(null);
		}

	return this;
}

public Object commute(IFn fn, ISeq args) throws Exception{
	if(errors != null)
		{
		throw new Exception("IRef has errors", (Exception) RT.first(errors));
		}
	Action action = new Action(this, fn, args);
	LockingTransaction trans = LockingTransaction.getRunning();
	if(trans != null)
		trans.enqueue(action);
	else if(nested.get() != null)
		{
		nested.set(nested.get().cons(action));
		}
	else
		enqueue(action);

	return this;
}

public Object set(Object val) throws Exception{
	synchronized(this)
		{
		if(altering)
			throw new Exception("Recursive change");
		LockingTransaction trans = LockingTransaction.getRunning();
		if(trans != null)
			throw new Exception("Cannot set an IRef in a transaction");
		if(inAlter.get() != null)
			throw new Exception("Cannot nest alters, use commute");
		setState(val);
		return val;
		}
}

void enqueue(Action action){
	synchronized(this)
		{
		if(busy)
			q.add(action);
		else
			{
			busy = true;
			executor.execute(action);
			}
		}
}

}
