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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

public class Agent extends ARef {

static class ActionQueue {
	public final IPersistentStack q;
	public final Throwable error; // non-null indicates fail state
	static final ActionQueue EMPTY = new ActionQueue(PersistentQueue.EMPTY, null);

	public ActionQueue( IPersistentStack q, Throwable error )
		{
		this.q = q;
		this.error = error;
		}
}

static final Keyword CONTINUE = Keyword.intern(null, "continue");
static final Keyword FAIL = Keyword.intern(null, "fail");

volatile Object state;
    AtomicReference<ActionQueue> aq = new AtomicReference<ActionQueue>(ActionQueue.EMPTY);

    volatile Keyword errorMode = CONTINUE;
    volatile IFn errorHandler = null;

final private static AtomicLong sendThreadPoolCounter = new AtomicLong(0);

final private static AtomicLong sendOffThreadPoolCounter = new AtomicLong(0);

final public static ExecutorService pooledExecutor =
	Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(), 
		createThreadFactory("clojure-agent-send-pool-%d", sendThreadPoolCounter));

final public static ExecutorService soloExecutor = Executors.newCachedThreadPool(
	createThreadFactory("clojure-agent-send-off-pool-%d", sendOffThreadPoolCounter));

final static ThreadLocal<IPersistentVector> nested = new ThreadLocal<IPersistentVector>();

private static ThreadFactory createThreadFactory(final String format, final AtomicLong threadPoolCounter) {
	return new ThreadFactory() {
		public Thread newThread(Runnable runnable) {
			Thread thread = new Thread(runnable);
			thread.setName(String.format(format, threadPoolCounter.getAndIncrement()));
			return thread;
		}
	};
}

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
		try
			{
			if(solo)
				soloExecutor.execute(this);
			else
				pooledExecutor.execute(this);
			}
		catch(Throwable error)
			{
			if(agent.errorHandler != null)
				{
				try
					{
					agent.errorHandler.invoke(agent, error);
					}
				catch(Throwable e) {} // ignore errorHandler errors
				}
			}
	}

	static void doRun(Action action){
		try
			{
			nested.set(PersistentVector.EMPTY);

			Throwable error = null;
			try
				{
				Object oldval = action.agent.state;
				Object newval =  action.fn.applyTo(RT.cons(action.agent.state, action.args));
				action.agent.setState(newval);
                action.agent.notifyWatches(oldval,newval);
				}
			catch(Throwable e)
				{
				error = e;
				}

			if(error == null)
				{
				releasePendingSends();
				}
			else
				{
				nested.set(null); // allow errorHandler to send
				if(action.agent.errorHandler != null)
					{
					try
						{
						action.agent.errorHandler.invoke(action.agent, error);
						}
					catch(Throwable e) {} // ignore errorHandler errors
					}
				if(action.agent.errorMode == CONTINUE)
					{
					error = null;
					}
				}

			boolean popped = false;
			ActionQueue next = null;
			while(!popped)
				{
				ActionQueue prior = action.agent.aq.get();
				next = new ActionQueue(prior.q.pop(), error);
				popped = action.agent.aq.compareAndSet(prior, next);
				}

			if(error == null && next.q.count() > 0)
				((Action) next.q.peek()).execute();
			}
		finally
			{
			nested.set(null);
			}
	}

	public void run(){
		doRun(this);
	}
}

public Agent(Object state) {
	this(state,null);
}

public Agent(Object state, IPersistentMap meta)  {
    super(meta);
    setState(state);
}

boolean setState(Object newState) {
	validate(newState);
	boolean ret = state != newState;
	state = newState;
	return ret;
}

public Object deref() {
	return state;
}

public Throwable getError(){
	return aq.get().error;
}

public void setErrorMode(Keyword k){
	errorMode = k;
}

public Keyword getErrorMode(){
	return errorMode;
}

public void setErrorHandler(IFn f){
	errorHandler = f;
}

public IFn getErrorHandler(){
	return errorHandler;
}

synchronized public Object restart(Object newState, boolean clearActions){
	if(getError() == null)
		{
		throw Util.runtimeException("Agent does not need a restart");
		}
	validate(newState);
	state = newState;

	if(clearActions)
		aq.set(ActionQueue.EMPTY);
	else
		{
		boolean restarted = false;
		ActionQueue prior = null;
		while(!restarted)
			{
			prior = aq.get();
			restarted = aq.compareAndSet(prior, new ActionQueue(prior.q, null));
			}

		if(prior.q.count() > 0)
			((Action) prior.q.peek()).execute();
		}

	return newState;
}

public Object dispatch(IFn fn, ISeq args, boolean solo) {
	Throwable error = getError();
	if(error != null)
		{
		throw Util.runtimeException("Agent is failed, needs restart", error);
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
	ActionQueue prior = null;
	while(!queued)
		{
		prior = aq.get();
		queued = aq.compareAndSet(prior, new ActionQueue((IPersistentStack)prior.q.cons(action), prior.error));
		}

	if(prior.q.count() == 0 && prior.error == null)
		action.execute();
}

public int getQueueCount(){
	return aq.get().q.count();
}

static public int releasePendingSends(){
	IPersistentVector sends = nested.get();
	if(sends == null)
		return 0;
	for(int i=0;i<sends.count();i++)
		{
		Action a = (Action) sends.valAt(i);
		a.agent.enqueue(a);
		}
	nested.set(PersistentVector.EMPTY);
	return sends.count();
}
}
