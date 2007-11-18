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
import java.util.concurrent.LinkedBlockingQueue;

public class Actor extends RestFn{
volatile Object state;
final Queue q = new LinkedList();
boolean busy = false;

//todo - make tuneable
final public static Queue errors = new LinkedBlockingQueue();
final static Executor executor = Executors.newCachedThreadPool();
final static ThreadLocal<PersistentVector> nested = new ThreadLocal<PersistentVector>();

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
			actor.state = fn.applyTo(RT.cons(actor, args));
			}
		catch(Exception e)
			{
			//todo report/callback
			errors.add(e);
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
	super(1);
	this.state = state;
}

public Object getState(){
	return state;
}

public Object doInvoke(Object fn, Object args){
	Action action = new Action(this, (IFn) fn, (ISeq) args);
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
