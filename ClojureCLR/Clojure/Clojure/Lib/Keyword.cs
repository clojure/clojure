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
    /// Represents a keyword
    /// </summary>
    public class Keyword: AFn, Named, IComparable  // ??JAVA only used IFn, not AFn.  NOt sure why.
    {
        #region Data

        /// <summary>
        /// The symbol giving the namespace/name (without :) of the keyword.
        /// </summary>
        private readonly Symbol _sym;

        /// <summary>
        /// Map from symbol to keyword to uniquify keywords.
        /// </summary>
        /// <remarks>Why introduce the JavaConcurrentDictionary?  
        /// We really only need a synchronized hash table with one operation: PutIfAbsent.</remarks>
        private static JavaConcurrentDictionary<Symbol,Keyword> _symKeyMap 
            = new JavaConcurrentDictionary<Symbol,Keyword>();

        protected Symbol Symbol
        {
          get { return _sym; }
        }

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Create (or find existing) keyword with given symbol's namespace/name.
        /// </summary>
        /// <param name="sym">The symbol giving the keyword's namespace/name.</param>
        /// <returns>A keyword</returns>
        public static Keyword intern(Symbol sym)
        {
            Keyword k = new Keyword(sym);
            Keyword existing = _symKeyMap.PutIfAbsent(sym, k);
            return existing == null ? k : existing;
        }

        /// <summary>
        /// Create (or find existing) keyword with given namespace/name.
        /// </summary>
        /// <param name="ns">The keyword's namespace name.</param>
        /// <param name="name">The keyword's name.</param>
        /// <returns>A keyword</returns>
        public static Keyword intern(string ns, string name)
        {
            return intern(Symbol.intern(ns, name));
        }

        /// <summary>
        /// Construct a keyword based on a symbol.
        /// </summary>
        /// <param name="sym">A symbol giving namespace/name.</param>
        private Keyword(Symbol sym)
        {
            this._sym = sym;
        }


        #endregion

        #region Object overrides

        /// <summary>
        /// Returns a string representing the keyword.
        /// </summary>
        /// <returns>A string representing the keyword.</returns>
        public override string ToString()
        {
            return ":" + _sym.ToString();
        }

        /// <summary>
        /// Determines if an object is equal to this keyword.  Value semantics.
        /// </summary>
        /// <param name="obj">The object to compare to.</param>
        /// <returns><value>true</value> if equal; <value>false</value> otherwise.</returns>
        public override bool Equals(object obj)
        {
            if ( obj == this ) 
                return true;

            Keyword keyword = obj as Keyword;

            if (keyword == null)
                return false;

            return _sym.Equals(keyword.Symbol);
        }

        /// <summary>
        /// Gets a hash code for the keyword.
        /// </summary>
        /// <returns>A hash code.</returns>
        public override int GetHashCode()
        {
            return Util.HashCombine(0,_sym.GetHashCode());
        }

        #endregion

        #region Named Members

        //  I prefer to use the following internally.

        /// <summary>
        /// The namespace name.
        /// </summary>
        public string Namespace
        {
            get { return _sym.Namespace; }
        }

        /// <summary>
        /// The name.
        /// </summary>
        public string Name
        {
            get { return _sym.Name; }
        }

        // the following are in the interface


        /// <summary>
        /// Gets the namespace name.
        /// </summary>
        /// <returns>The namespace name.</returns>
        public string getNamespace()
        {
            return _sym.Namespace;
        }

        /// <summary>
        /// Gets the name.
        /// </summary>
        /// <returns>The name.</returns>
        public string getName()
        {
            return _sym.Name;
        }

        #endregion

        #region IFn Members

        /// <summary>
        /// (:keyword arg)  => (get arg :keyword)
        /// </summary>
        /// <param name="arg1">The object to access.</param>
        /// <returns>The value mapped to the keyword.</returns>
        public override object invoke(object arg1)
        {
            return RT.get(arg1, this);
        }


        /// <summary>
        /// (:keyword arg default) => (get arg :keyword default)
        /// </summary>
        /// <param name="arg1">The object to access.</param>
        /// <param name="arg2">Default value if not found.</param>
        /// <returns></returns>
        public override object invoke(object arg1, object arg2)
        {
            return RT.get(arg1, this, arg2);
        }


        #endregion

        #region IComparable Members

        /// <summary>
        /// Compare this to another object.
        /// </summary>
        /// <param name="obj">The object to compare to.</param>
        /// <returns>neg,zero,pos for &lt; = &gt;</returns>
        public int CompareTo(object obj)
        {
            return _sym.CompareTo(((Keyword)obj)._sym);
        }

        #endregion
    }
}
