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
using System.Runtime.CompilerServices;

namespace clojure.lang
{

    /// <summary>
    /// Provides basic implementation for the <see cref="IRef">IRef</see> interface methods.
    /// </summary>
    public abstract class ARef : AReference, IRef
    {
        #region Data

        /// <summary>
        /// The validator for the reference.
        /// </summary>
        protected volatile IFn _validator = null;

        /// <summary>
        /// The set of watchers for the reference.
        /// </summary>
        private volatile IPersistentMap _watchers = PersistentHashMap.EMPTY;

        #endregion

        #region Ctors and factory methods

        /// <summary>
        /// Initializes an <see cref="ARef">ARef</see> with null metadata.
        /// </summary>
        public ARef()
            : base()
        {
        }

        /// <summary>
        ///  Initializes an <see cref="ARef">ARef</see> with the given metadata.
        /// </summary>
        /// <param name="meta">The metadata to use</param>
        public ARef(IPersistentMap meta)
            : base(meta)
        {
        }
       

        #endregion

        #region IDeref Members

        /// <summary>
        /// Gets the (immutable) value the reference is holding.
        /// </summary>
        /// <returns>The value</returns>
        public abstract object deref();

        #endregion

        #region IRef Members

        /// <summary>
        /// Invoke an <see cref="IFn">IFn</see> on a value to validate it.
        /// </summary>
        /// <param name="vf">The <see cref="IFn">IFn</see> to invoke.</param>
        /// <param name="val">The value to validate.</param>
        /// <remarks>Uneventful return marks a successful validation.  
        /// To indicate a failed validation, the validation function should return <value>false</value> or throw an exception.
        /// <para>This appears in multiple places.  Should find it a common home?</para></remarks>
        protected internal static void Validate(IFn vf, object val)
        {
            if (vf == null)
                return;

            bool ret = false;

            try
            {
               ret = RT.booleanCast(vf.invoke(val));
            }
            catch (Exception e)
            {
                throw new InvalidOperationException("Invalid reference state", e);
            }

            if ( ! ret )
                throw new InvalidOperationException("Invalid reference state");
        }

        /// <summary>
        /// Call the reference's validator on the given value.
        /// </summary>
        /// <param name="val">The value to validate</param>
        protected internal void Validate(object val)
        {
            Validate(_validator, val);
        }

        /// <summary>
        /// Sets the validator.
        /// </summary>
        /// <param name="vf">The new validtor</param>
        /// <remarks>The current value must validate in order for this validator to be accepted.  If not, an exception will be thrown.</remarks>
        public virtual void setValidator(IFn vf)
        {
            Validate(vf, deref());
            _validator = vf;
        }

        /// <summary>
        /// Gets the validator.
        /// </summary>
        /// <returns>The current validator.</returns>
         public IFn getValidator()
        {
            return _validator;
        }

        #endregion

        #region Watches
        
        /// <summary>
        /// Gets a map of watchers (key=Agent, value=IFn).
        /// </summary>
        /// <returns>An immutable map of watchers (key=Agent, value=IFn). </returns>
        public IPersistentMap getWatches()
        {
            return _watchers;
        }


         /// <summary>
         /// Adds a new watcher.
         /// </summary>
         /// <param name="watcher">The <see cref="Agent">Agent</see> doing the watching.</param>
         /// <param name="action">The 'message' to send when the value changes.</param>
         /// <param name="sendOff">If true, use <see cref="Agent.sendOff">send-off</see> to send the message, else use <see cref="Agent.send()">send</see>.</param>
         /// <returns></returns>
         [MethodImpl( MethodImplOptions.Synchronized)]
         public IRef addWatch(Agent watcher, IFn action, bool sendOff)
         {
            _watchers = _watchers.assoc(watcher, new object[] { action, sendOff });
             return this;
         }


         /// <summary>
         /// Remove a watcher.
         /// </summary>
         /// <param name="watcher">The <see cref="Agent">Agent</see> to be removed.</param>
         /// <returns>This IRef (for chaining).</returns>
         [MethodImpl(MethodImplOptions.Synchronized)]
         public IRef removeWatch(Agent watcher)
         {
             _watchers = _watchers.without(watcher);
             return this;
         }


         /// <summary>
         /// Notify all watchers.
         /// </summary>
         public void notifyWatches()
         {
             IPersistentMap ws = _watchers;
             if (ws.count() > 0)
             {
                 ISeq args = new Cons(this, null);
                 for (ISeq s = RT.seq(ws); s != null; s = s.rest())
                 {
                     IMapEntry me = (IMapEntry)s.first();
                     object[] a = (object[])me.val();
                     Agent agent = (Agent)me.key();
                     try
                     {
                         agent.dispatch((IFn)a[0], args, (Boolean)a[1]);
                     }
                     catch (Exception)
                     {
                         // eat dispatching exceptions and continue
                     }
                 }
             }
         }

        #endregion
    }
}
