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

namespace clojure.lang
{
    /// <summary>
    /// Represents a reference to a value.
    /// </summary>
    /// <remarks>
    /// <para><see cref="IRef">IRef</see> is the basic interface supported 
    /// by <see cref="Ref">Ref</see>s, <see cref="Agent">Agent</see>s, 
    /// <see cref="Atom">Atom</see>s, <see cref="Var">Var</see>s, and 
    /// other references to values.  Many</para>
    /// <para>This interface supports  
    /// getting/setting the validator for the value, and getting/setting watchers.  
    /// Dereferencing is supplied in interface <see cref="IDeref">IDeref</see>.
    /// This interface does not support changes to values.  Changes are the responsibility of the implementations of this interface,
    /// and often have to be done in concert with <see cref="LockingTransaction">LockingTransaction</see>.</para>
    /// <para>The validator function will be applied to any new value before that value is applied.  
    /// If the validator throws an exception or returns false, changing the reference to the new value is aborted.  
    /// When setting a new validator, it must validate the current value.</para>
    /// <para>A reference can be watched by one or more <see cref="Agent">Agent</see>s. The agent will be sent a message when the value changes.</para>
    /// </remarks>
    public interface IRef : IDeref
    {
        /// <summary>
        /// Sets the validator.
        /// </summary>
        /// <param name="vf">The new validtor</param>
        void setValidator(IFn vf);

        /// <summary>
        /// Gets the validator.
        /// </summary>
        /// <returns>The current validator.</returns>
        IFn getValidator();


        /// <summary>
        /// Gets a map of watchers (key=Agent, value=IFn).
        /// </summary>
        /// <returns>A (immutable) map of watchers (key=Agent, value=IFn). </returns>
        IPersistentMap getWatches();

        /// <summary>
        /// Adds a new watcher.
        /// </summary>
        /// <param name="watcher">The <see cref="Agent">Agent</see> doing the watching.</param>
        /// <param name="action">The 'message' to send when the value changes.</param>
        /// <param name="sendOff">If true, use <see cref="Agent.sendOff">send-off</see> to send the message, else use <see cref="Agent.send()">send</see>.</param>
        /// <returns></returns>
        IRef addWatch(Agent watcher, IFn action, bool sendOff);

        /// <summary>
        /// Remove a watcher.
        /// </summary>
        /// <param name="watcher">The <see cref="Agent">Agent</see> to be removed.</param>
        /// <returns>This IRef (for chaining).</returns>
        IRef removeWatch(Agent watcher);

        /// <summary>
        /// Notify all watchers.
        /// </summary>
        void notifyWatches();
    }
}
