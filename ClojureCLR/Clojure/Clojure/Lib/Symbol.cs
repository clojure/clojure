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
    /// Represents a symbol.
    /// </summary>
    /// <remarks>See the Clojure documentation for more information.</remarks>
    [Serializable]
    public class Symbol: AFn, Named, IComparable   
    {
        #region Instance variables

        /// <summary>
        /// The name of the namespace for this symbol (if namespace-qualified).
        /// </summary>
        /// <remarks>This string must be interned.</remarks>
        protected readonly  string _ns;

        /// <summary>
        /// The name of the symbol.
        /// </summary>
        /// <remarks>This string must be interned.</remarks>
        protected readonly string _name;

        /// <summary>
        /// The cached hashcode.
        /// </summary>
        protected readonly int _hash;

        #endregion

        #region C-tors & factory methods

        /// <summary>
        /// Intern a symbol with the given name  and namespace-name.
        /// </summary>
        /// <param name="ns">The name of the namespace.</param>
        /// <param name="name">The name of the symbol.</param>
        /// <returns>A new symbol.</returns>
        /// <remarks>
        /// Interning here does not imply uniquifying.  
        /// The strings for the namespace-name and the symbol-name are uniquified.
        /// </remarks>
        static public Symbol intern(string ns, string name)
        {
            return new Symbol(ns == null ? null : String.Intern(ns), String.Intern(name));
        }

        /// <summary>
        /// Intern a symbol with the given name (extracting the namespace if name is of the form ns/name).
        /// </summary>
        /// <param name="nsname">The (possibly qualified) name</param>
        /// <returns>A new symbol.</returns>
        static public Symbol intern(string nsname)
        {
            int i = nsname.LastIndexOf('/');
            return i == -1
                ? new Symbol(null, String.Intern(nsname))
                : new Symbol(String.Intern(nsname.Substring(0, i)),
                             String.Intern(nsname.Substring(i + 1)));
        }

        /// <summary>
        /// Create a symbol, no namespace, from an interned string.
        /// </summary>
        /// <param name="name_interned">The name (interned string)</param>
        /// <returns></returns>
        static public Symbol create(string name_interned)
        {
            return new Symbol(null, name_interned);
        }

        /// <summary>
        /// Create a symbol with given namespace name and name, both interned.
        /// </summary>
        /// <param name="ns_interned">Interned namespace name.</param>
        /// <param name="name_interned">Interned symbol name.</param>
        /// <returns></returns>
        static public Symbol create(string ns_interned, string name_interned)
        {
            return new Symbol(ns_interned, name_interned);
        }

        /// <summary>
        /// Construct a symbol from interned namespace name and symbol name.
        /// </summary>
        /// <param name="ns_interned">The (interned) namespace name.</param>
        /// <param name="name_interned">The (interned) symbol name.</param>
        private Symbol(string ns_interned, string name_interned) 
            : base()
        {
            this._name = name_interned;
            this._ns = ns_interned;
            this._hash = ComputeHashCode();
        }

        /// <summary>
        /// Construct a symbol from interned namespace name and symbol name,  with given metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="ns_interned">The (interned) namespace name.</param>
        /// <param name="name_interned">The (interned) symbol name.</param>
        private Symbol(IPersistentMap meta, string ns_interned, string name_interned)
            : base(meta)
        {
            this._name = name_interned;
            this._ns = ns_interned;
            this._hash = ComputeHashCode();
        }

        /// <summary>
        /// Compute the hash code for the symbol.
        /// </summary>
        /// <returns>The hash code.</returns>
        private int ComputeHashCode()
        {
            return Util.HashCombine(_name.GetHashCode(), Util.Hash(_ns));
        }


        #endregion

        #region Object overrides

        /// <summary>
        /// Return  a string representing the symbol.
        /// </summary>
        /// <returns>A string representing the symbol.</returns>
        public override string ToString()
        {
            return _ns == null ? _name : _ns + "/" + _name;
        }

        /// <summary>
        /// Determine if an object is equal to this symbol.
        /// </summary>
        /// <param name="obj">The object to compare to.</param>
        /// <returns><value>true</value> if they are the same;<value>false</value> otherwise.</returns>
        /// <remarks>Uses value semantics, value determined by namespace name and symbol name.</remarks>
        public override bool Equals(object obj)
        {
            if (this == obj)
                return true;

            Symbol sym = obj as Symbol;

            if (sym == null)
                return false;

            // interned strings, use identity compare
            return (Object.ReferenceEquals(Name,sym.Name) && (Object.ReferenceEquals(Namespace,sym.Namespace)));
        }

        /// <summary>
        /// Get the hash code.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return _hash;
        }

        #endregion

        #region IObj members

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap meta)
        {
            // Java did not do identity test.
            return meta == _meta
                ? this
                : new Symbol(meta, _ns, _name);
        }

        #endregion

        #region Named members

        // I prefer to use these internally.

        /// <summary>
        /// Get the namespace name.
        /// </summary>
        public string Namespace
        {
            get { return _ns; }
        }

        /// <summary>
        /// Get the symbol name.
        /// </summary>
        public string Name
        {
            get { return _name; }
        } 

        // the following are in the interface
        
        /// <summary>
        /// Get the namespace name.
        /// </summary>
        /// <returns>The namespace name.</returns>
        public string getNamespace()
        {
            return _ns;
        }

        /// <summary>
        /// Gets the symbol name.
        /// </summary>
        /// <returns>The symbol name.</returns>
        public string getName()
        {
            return _name;
        }

        #endregion

        #region IFn members


        public override object invoke(Object obj)
        {
            return RT.get(obj, this);
        }

        public override object invoke(Object obj, Object notFound)
        {
            return RT.get(obj, this, notFound);
        }

        #endregion

        #region IComparable Members

        /// <summary>
        /// Compare this symbol to another object.
        /// </summary>
        /// <param name="obj">The object to comapre to.</param>
        /// <returns>neg,zero,pos semantics.</returns>
        public int CompareTo(object obj)
        {
            Symbol s = obj as Symbol;
            if (s == null)
                throw new InvalidOperationException("Can't compare to null.");
            if (Equals(s))
                return 0;
            if (_ns == null && s._ns != null)
                return -1;
            if (_ns != null)
            {
                if (s._ns == null)
                    return 1;
                int nsc = _ns.CompareTo(s._ns);
                if (nsc != 0)
                    return nsc;
            }
            return _name.CompareTo(s._name);
        }

        #endregion

        #region Other

        ///// <summary>
        ///// Create a copy of this symbol.
        ///// </summary>
        ///// <returns>A copy of this symbol.</returns>
        //private object readResolve()
        //{
        //    return intern(_ns, _name);
        //}

        #endregion
    }
}
