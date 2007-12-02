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

public class Actor implements Ref{
volatile Object state;
final Queue q = new LinkedList();
boolean busy = false;
boolean commuting = false;

volatile ISeq errors = null;
//todo - make tuneable
final static Executor executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors());
//final static Executor executor = Executors.newCachedThreadPool();
final static ThreadLocal<PersistentVector> nested = new ThreadLocal<PersistentVector>();
final static ThreadLocal inChange = new ThreadLocal();

static class Action implements Runnable{
	final Actor actor;
	final IFn fn;
	final ISeq args;


	public Action(Actor actor, IFn fn, ISeq args){
		this.actor = actor;
		this.args = args;
		this.fn = fn;
	}

	public void run(){
		nested.set(PersistentVector.EMPTY);
		boolean hadError = false;
		try
			{
			actor.doAlter(fn, args);
			}
		catch(Exception e)
			{
			//todo report/callback
			actor.errors = RT.cons(e, actor.errors);
			hadError = true;
			}

		if(!hadError)
			{
			for(ISeq s = nested.get().seq(); s != null; s = s.rest())
				{
				Action a = (Action) s.first();
				a.actor.enqueue(a);
				}
			}

		synchronized(actor)
			{
			if(!actor.q.isEmpty())
				{
				executor.execute((Runnable) actor.q.remove());
				}
			else
				{
				actor.busy = false;
				}
			}

		nested.set(null);
	}
}

public Actor(Object state){
	setState(state);
}

void setState(Object newState){
	if(newState instanceof IObj)
		{
		IObj o = (IObj) newState;
		if(RT.get(o.meta(), RT.ACTOR_KEY) != this)
			{
			newState = o.withMeta((IPersistentMap) RT.assoc(o.meta(), RT.ACTOR_KEY, this));
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
		commuting = true;
		setState(fn.applyTo(RT.cons(state, args)));
		}
	finally
		{
		commuting = false;
		}
}

public Object alter(IFn fn, ISeq args) throws Exception{
	if(errors != null)
		{
		throw new Exception("Actor has errors", (Exception) RT.first(errors));
		}
	//Action action = new Action(this, fn, args);
	if(commuting)
		throw new Exception("Recursive change");
	LockingTransaction trans = LockingTransaction.getRunning();
	if(trans != null)
		throw new Exception("Cannot change an Actor in a transaction");
	if(inChange.get() != null)
		throw new Exception("Cannot nest changes, use send");

	try
		{
		inChange.set(this);
		doAlter(fn, args);
		}
	finally
		{
		inChange.set(null);
		}

	return this;
}

public Object commute(IFn fn, ISeq args) throws Exception{
	if(errors != null)
		{
		throw new Exception("Actor has errors", (Exception) RT.first(errors));
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
