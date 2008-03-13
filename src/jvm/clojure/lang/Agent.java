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

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class Agent implements IRef{
volatile Object state;
AtomicReference<IPersistentStack> q = new AtomicReference(PersistentQueue.EMPTY);

volatile ISeq errors = null;

final public static Executor pooledExecutor =
		Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

final static Executor soloExecutor = Executors.newCachedThreadPool();

final static ThreadLocal<PersistentVector> nested = new ThreadLocal<PersistentVector>();


static class Action implements Runnable{
	final Agent agent;
	final IFn fn;
	final ISeq args;
	final boolean solo;


	public Action(Agent agent, IFn fn, ISeq args, boolean solo){
		this.agent = agent;
		this.args = args;
		this.fn = fn;
		this.solo = solo;
	}

	void execute(){
		if(solo)
			soloExecutor.execute(this);
		else
			pooledExecutor.execute(this);
	}

	static void doRun(Action action){
		try
			{
			Var.pushThreadBindings(RT.map(RT.AGENT, action.agent));
			nested.set(PersistentVector.EMPTY);

			boolean hadError = false;
			try
				{
				action.agent.setState(action.fn.applyTo(RT.cons(action.agent.state, action.args)));
				}
			catch(Exception e)
				{
				//todo report/callback
				action.agent.errors = RT.cons(e, action.agent.errors);
				hadError = true;
				}

			if(!hadError)
				{
				for(ISeq s = nested.get().seq(); s != null; s = s.rest())
					{
					Action a = (Action) s.first();
					a.agent.enqueue(a);
					}
				}

			boolean popped = false;
			IPersistentStack next = null;
			while(!popped)
				{
				IPersistentStack prior = action.agent.q.get();
				next = prior.pop();
				popped = action.agent.q.compareAndSet(prior, next);
				}

			if(next.count() > 0)
				((Action) next.peek()).execute();

			}
		finally
			{
			nested.set(null);
			Var.popThreadBindings();
			}
	}

	public void run(){
		doRun(this);
	}
}

public Agent(Object state){
	setState(state);
}

void setState(Object newState){
	state = newState;
}

public Object get() throws Exception{
	if(errors != null)
		{
		throw new Exception("Agent has errors", (Exception) RT.first(errors));
		}
	return state;
}

public ISeq getErrors(){
	return errors;
}

public void clearErrors(){
	errors = null;
}

public Object dispatch(IFn fn, ISeq args, boolean solo) throws Exception{
	if(errors != null)
		{
		throw new Exception("Agent has errors", (Exception) RT.first(errors));
		}
	Action action = new Action(this, fn, args, solo);
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
	boolean queued = false;
	IPersistentStack prior = null;
	while(!queued)
		{
		prior = q.get();
		queued = q.compareAndSet(prior, (IPersistentStack) prior.cons(action));
		}

	if(prior.count() == 0)
		action.execute();
}

}
