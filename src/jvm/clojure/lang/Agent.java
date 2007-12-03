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
import java.util.concurrent.*;

public class Agent implements IRef{
volatile Object state;
final Queue q = new LinkedList();
boolean busy = false;

volatile ISeq errors = null;
//todo - make tuneable
final public static ThreadPoolExecutor executor =
		new ThreadPoolExecutor(2 * Runtime.getRuntime().availableProcessors(),
		                       2 * Runtime.getRuntime().availableProcessors(),
		                       0L, TimeUnit.MILLISECONDS,
		                       new LinkedBlockingQueue<Runnable>());
//Executors.newFixedThreadPool(2 * Runtime.getRuntime().availableProcessors());
//final static Executor executor = Executors.newCachedThreadPool();
final static ThreadLocal<PersistentVector> nested = new ThreadLocal<PersistentVector>();

static class Action implements Runnable{
	final Agent agent;
	final IFn fn;
	final ISeq args;


	public Action(Agent agent, IFn fn, ISeq args){
		this.agent = agent;
		this.args = args;
		this.fn = fn;
	}

	public void run(){
		nested.set(PersistentVector.EMPTY);
		boolean hadError = false;
		try
			{
			agent.setState(fn.applyTo(RT.cons(agent.state, args)));
			}
		catch(Exception e)
			{
			//todo report/callback
			agent.errors = RT.cons(e, agent.errors);
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

		synchronized(agent)
			{
			if(!agent.q.isEmpty())
				{
				executor.execute((Runnable) agent.q.remove());
				}
			else
				{
				agent.busy = false;
				}
			}

		nested.set(null);
	}
}

public Agent(Object state){
	setState(state);
}

void setState(Object newState){
	if(newState instanceof IObj)
		{
		IObj o = (IObj) newState;
		if(RT.get(o.meta(), RT.AGENT_KEY) != this)
			{
			newState = o.withMeta((IPersistentMap) RT.assoc(o.meta(), RT.AGENT_KEY, this));
			}
		}
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

public Object dispatch(IFn fn, ISeq args) throws Exception{
	if(errors != null)
		{
		throw new Exception("Agent has errors", (Exception) RT.first(errors));
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
