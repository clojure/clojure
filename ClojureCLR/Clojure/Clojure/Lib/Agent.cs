/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace clojure.lang
{
    /// <summary>
    /// Represents an Agent.
    /// </summary>
    /// <remarks>
    /// <para>See the Clojure documentation for more information.</para>
    /// <para>The Java implementation plays many more games with thread pools.  The CLR does not provide such support. We need to revisit this in CLR 4.  
    /// Until then: TODO: Implement our own thread pooling?</para>
    /// </remarks>
    public sealed class Agent : ARef
    {
        #region Data

        /// <summary>
        /// The current state of the agent.
        /// </summary>
        private volatile object _state;

        /// <summary>
        /// The current state of the agent.
        /// </summary>
        public object State
        {
          get { return _state; }
        }

        /// <summary>
        /// A queue of pending actions.
        /// </summary>
        private AtomicReference<IPersistentStack> _q = new AtomicReference<IPersistentStack>(PersistentQueue.EMPTY);

        /// <summary>
        /// Number of items in the queue.
        /// </summary>
        public int QueueCount
        {
            get
            {
                return _q.Get().count();
            }
        }


        /// <summary>
        /// Agent errors, a sequence of Exceptions.
        /// </summary>
        private volatile ISeq _errors = null;

        /// <summary>
        /// Agent errors, a sequence of Exceptions.
        /// </summary>
        public ISeq Errors
        {
            get { return _errors; }
        }


        /// <summary>
        /// Add an error.
        /// </summary>
        /// <param name="e">The exception to add.</param>
        public void AddError(Exception e)
        {
            _errors = RT.cons(e, _errors);
        }


        /// <summary>
        /// A collection of agent actions enqueued during the current transaction.  Per thread.
        /// </summary>
        [ThreadStatic]
        private static IPersistentVector _nested;

        /// <summary>
        /// A collection of agent actions enqueued during the current transaction.  Per thread.
        /// </summary>
        public static IPersistentVector Nested
        {
            get { return _nested; }
            set { _nested = value; }
        }

        #endregion

        #region C-tors & factory methods

        /// <summary>
        /// Construct an agent with given state and null metadata.
        /// </summary>
        /// <param name="state">The initial state.</param>
        public Agent(object state)
            : this(state, null)
        {
        }

        /// <summary>
        /// Construct an agent with given state and metadata.
        /// </summary>
        /// <param name="state">The initial state.</param>
        /// <param name="meta">The metadata to attach.</param>
        public Agent(Object state, IPersistentMap meta)
            :base(meta)
        {
            SetState(state);
        }
        
        #endregion

        #region State manipulation
        
        /// <summary>
        /// Set the state.
        /// </summary>
        /// <param name="newState">The new state.</param>
        /// <returns><value>true</value> if the state changed; <value>false</value> otherwise.</returns>
        private bool SetState(object newState)
        {
            Validate(newState);
            bool ret = _state != newState;
            _state = newState;
            return ret;
        }

        #endregion

        #region Agent methods

        /// <summary>
        /// Get the agent's errors.
        /// </summary>
        /// <returns>A sequence of the errors.</returns>
        /// <remarks>Lowercase-name (and is a method instead of a property) for core.clj compatibility.</remarks>
        public ISeq getErrors()
        {
            return _errors;
        }

        /// <summary>
        /// Clear the agent's errors.
        /// </summary>
        /// <remarks>Lowercase-name and  for core.clj compatibility.</remarks>
        public void clearErrors()
        {
            _errors = null;
        }

        /// <summary>
        /// Send a message to the agent.
        /// </summary>
        /// <param name="fn">The function to be called on the current state and the supplied arguments.</param>
        /// <param name="args">The extra arguments to the function.</param>
        /// <param name="solo"><value>true</value> means execute on its own thread (send-off); 
        /// <value>false</value> means use a thread pool thread (send).</param>
        /// <returns>This agent.</returns>
        public object dispatch(IFn fn, ISeq args, Boolean solo)
        {
            if ( _errors != null )
                throw new Exception("Agent has errors", (Exception)RT.first(_errors));
            Action action = new Action(this,fn,args,solo);
            DispatchAction(action);

            return this;
        }

        /// <summary>
        /// Send an action (encapsulated message).
        /// </summary>
        /// <param name="action">The action to execute.</param>
        /// <remarks>
        /// <para>If there is a transaction running on this thread, 
        /// defer execution until the transaction ends 
        /// (enqueue the action on the transaction).</para>
        /// <para>If there is already an action running, enqueue it (nested).</para>
        /// <para>Otherwise, queue it for execution.</para>
        /// </remarks>
        internal static void DispatchAction(Action action)
        {
            LockingTransaction trans = LockingTransaction.getRunning();
            if (trans != null)
                trans.enqueue(action);
            else if (_nested != null)
                _nested = _nested.cons(action);
            else
                action.Agent.Enqueue(action);
        }

        /// <summary>
        /// Enqueue an action in the pending queue.
        /// </summary>
        /// <param name="action">The action to enqueue.</param>
        /// <remarks>Spin-locks to update the queue.</remarks>
        void Enqueue(Action action)
        {
            bool queued = false;
            IPersistentStack prior = null;
            while (!queued)
            {
                prior = _q.Get();
                queued = _q.CompareAndSet(prior, (IPersistentStack)prior.cons(action));
            }

            if (prior.count() == 0)
                action.execute();
        }


        #endregion

        #region IDeref Members

        /// <summary>
        /// Gets the (immutable) value the reference is holding.
        /// </summary>
        /// <returns>The value</returns>
        public override object deref()
        {
            if (_errors != null)
                throw new Exception("Agent has errors", (Exception)RT.first(_errors));
            return _state;
        }

        #endregion

        #region core.clj compatability

        /// <summary>
        /// Shutdown all threads executing.
        /// </summary>
        /// <remarks>We need to work on this.</remarks>
        public static void shutdown()
        {
            // JAVA: soloExecutor.shutdown();
            // JAVA: pooledExecutor.shutdown();

            // TODO: record active jobs and shut them down?
        }
        #endregion

        /// <summary>
        /// An encapsulated message.
        /// </summary>
        internal sealed class Action
        {
            #region Data

            /// <summary>
            /// The agent this message is for.
            /// </summary>
            readonly Agent _agent;

            /// <summary>
            /// The agent this message is for.
            /// </summary>
            public Agent Agent
            {
                get { return _agent; }
            } 

            /// <summary>
            /// The function to call to create the new state.
            /// </summary>
            readonly IFn _fn;

            /// <summary>
            /// The arguments to call (in addition to the current state).
            /// </summary>
            readonly ISeq _args;

            /// <summary>
            /// Should execute on its own thread (not a thread-pool thread).
            /// </summary>
            readonly bool _solo;

            #endregion

            #region Ctors

            /// <summary>
            /// Create an encapsulated message to an agent.
            /// </summary>
            /// <param name="agent">The agent the message is for.</param>
            /// <param name="fn">The function to compute the new value.</param>
            /// <param name="args">Additional arguments (in addition to the current state).</param>
            /// <param name="solo">Execute on its own thread?</param>
            public Action(Agent agent, IFn fn, ISeq args, bool solo)
            {
                _agent = agent;
                _fn = fn;
                _args = args;
                _solo = solo;
            }

            #endregion

            #region Executing the action

            /// <summary>
            /// Send the message.
            /// </summary>
            public void execute()
            {
                if (_solo)
                {
                    // TODO:  Reuse/cleanup these threads
                    Thread thread = new Thread(ExecuteAction);
                    //thread.Priority = ThreadPriority.Lowest;
                    thread.Start(null);
                }
                else
                    ThreadPool.QueueUserWorkItem(ExecuteAction);
            }

            /// <summary>
            /// Worker method to execute the action on a thread.
            /// </summary>
            /// <param name="state">(not used)</param>
            /// <remarks>corresponds to doRun in Java version</remarks>
            void ExecuteAction(object state)
            {
                try
                {
                    Var.pushThreadBindings(RT.map(RT.AGENT, _agent));
                    Agent.Nested = PersistentVector.EMPTY;

                    bool hadError = false;

                    try
                    {
                        object oldval = _agent.State;
                        object newval = _fn.applyTo(RT.cons(_agent.State, _args));
                        _agent.SetState(newval);
                        _agent.notifyWatches(oldval,newval);
                    }
                    catch (Exception e)
                    {
                        // TODO: report/callback  (Java TODO)
                        _agent.AddError(e);
                        hadError = true;
                    }

                    if (!hadError)
                        releasePendingSends();

                    bool popped = false;
                    IPersistentStack next = null;
                    while (!popped)
                    {
                        IPersistentStack prior = _agent._q.Get();
                        next = prior.pop();
                        popped = _agent._q.CompareAndSet(prior, next);
                    }

                    if (next.count() > 0)
                        ((Action)next.peek()).execute();
                }
                finally
                {
                    Nested = null;
                    Var.popThreadBindings();
                }
            }

            #endregion
        }

        /// <summary>
        /// Enqueue nested actions.
        /// </summary>
        /// <returns></returns>
        /// <remarks>lowercase for core.clj compatibility</remarks>
        public static int releasePendingSends()
        {
            IPersistentVector sends = Agent.Nested;
            if (sends == null)
                return 0;
            for (int i = 0; i < sends.count(); i++)
            {
                Action a = (Action)sends.valAt(i);
                a.Agent.Enqueue(a);
            }
            Nested = PersistentVector.EMPTY;
            return sends.count();
        }
    }
}
