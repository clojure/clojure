/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Nov 17, 2007 */

package clojure.lang;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.Map;

public class Agent implements IRef{
volatile Object state;
volatile IFn validator = null;
AtomicReference<IPersistentStack> q = new AtomicReference(PersistentQueue.EMPTY);
AtomicReference<IPersistentMap> watchers = new AtomicReference(PersistentHashMap.EMPTY);

volatile ISeq errors = null;

final public static ExecutorService pooledExecutor =
		Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors());

final static ExecutorService soloExecutor = Executors.newCachedThreadPool();

final static ThreadLocal<IPersistentVector> nested = new ThreadLocal<IPersistentVector>();


public static void shutdown(){
	soloExecutor.shutdown();
	pooledExecutor.shutdown();
}

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
			boolean changed = false;
			try
				{
				changed = action.agent.setState(action.fn.applyTo(RT.cons(action.agent.state, action.args)));
				for(Object o : action.agent.watchers.get())
					{
					Map.Entry e = (Map.Entry) o;
					((IFn) e.getValue()).invoke(e.getKey(), action.agent, RT.box(changed));
					}
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

public Agent(Object state) throws Exception{
	this(state,null);
}

public Agent(Object state, IFn validator) throws Exception{
	this.validator = validator;
	setState(state);
}

boolean setState(Object newState) throws Exception{
	validate(getValidator(),newState);
	boolean ret = state != newState;
	state = newState;
	return ret;
}

void validate(IFn vf, Object val){
	try{
		if(vf != null)
			vf.invoke(val);
		}
	catch(Exception e)
		{
		throw new IllegalStateException("Invalid agent state", e);
		}
}

public Object get() throws Exception{
	if(errors != null)
		{
		throw new Exception("Agent has errors", (Exception) RT.first(errors));
		}
	return state;
}

public void setValidator(IFn vf){
	validate(vf,state);
	validator = vf;
}

public IFn getValidator(){
	return validator;
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
	dispatchAction(action);

	return this;
}

static void dispatchAction(Action action){
	LockingTransaction trans = LockingTransaction.getRunning();
	if(trans != null)
		trans.enqueue(action);
	else if(nested.get() != null)
		{
		nested.set(nested.get().cons(action));
		}
	else
		action.agent.enqueue(action);
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

public int getQueueCount(){
	return q.get().count();
}

public Agent addWatch(Object watcher, IFn callback){
	boolean added = false;
	IPersistentMap prior = null;
	while(!added)
		{
		prior = watchers.get();
		added = watchers.compareAndSet(prior, prior.assoc(watcher,callback));
		}

	return this;
}

public Agent removeWatch(Object watcher) throws Exception{
	boolean removed = false;
	IPersistentMap prior = null;
	while(!removed)
		{
		prior = watchers.get();
		removed = watchers.compareAndSet(prior, prior.without(watcher));
		}
	
	return this;
}

}
