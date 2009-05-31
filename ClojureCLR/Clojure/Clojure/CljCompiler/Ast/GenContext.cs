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
using System.Reflection.Emit;
using System.Reflection;
using Microsoft.Linq.Expressions;

namespace clojure.lang.CljCompiler.Ast
{

    enum CompilerMode { Immediate, File };

    class GenContext
    {
        #region Data

        readonly CompilerMode _mode;

        internal CompilerMode Mode
        {
            get { return _mode; }
        } 



        readonly AssemblyBuilder _assyBldr;
        public AssemblyBuilder AssyBldr
        {
            get { return _assyBldr; }
            //set { _ab = value; }
        }

        readonly ModuleBuilder _moduleBldr;
        public ModuleBuilder ModuleBldr
        {
            get { return _moduleBldr; }
            //set { _moduleBldr = value; }
        }

        //TypeBuilder _typeBldr;
        //public TypeBuilder TypeBldr
        //{
        //    get { return _typeBldr; }
        //    //set { _tb = value; }
        //}

        FnExpr _fnExpr = null;
        internal FnExpr FnExpr
        {
            get { return _fnExpr; }
            //set { _fnExpr = value; }
        }

        //Type _baseType;
        //public Type BaseType
        //{
        //    get { return _baseType; }
        //    //set { _baseType = value; }
        //}

        //ParameterExpression _thisFn;
        //public ParameterExpression ThisFn
        //{
        //    get { return _thisFn; }
        //    //set { _thisFn = value; }
        //}

        #endregion

        #region C-tors & factory methods

        public GenContext(string assyName, CompilerMode mode)
            : this(assyName,null,mode)
        {
        }

        public GenContext(string assyName, string directory, CompilerMode mode)
        {
            AssemblyName aname = new AssemblyName(assyName);
            _assyBldr = AppDomain.CurrentDomain.DefineDynamicAssembly(aname, AssemblyBuilderAccess.RunAndSave,directory);
            _moduleBldr = _assyBldr.DefineDynamicModule(aname.Name, aname.Name + ".dll", true);
            _mode = mode;
        }

        private GenContext(CompilerMode mode)
        {
            _mode = mode;
        }

        public GenContext CreateWithNewType(FnExpr fnExpr)
        {
            GenContext newContext = Clone();
            newContext._fnExpr = fnExpr;
             return newContext;
        }

        private GenContext Clone()
        {
            return (GenContext) this.MemberwiseClone();
        }

        #endregion
    }
}
