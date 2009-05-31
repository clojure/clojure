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
using System.Reflection;
using Microsoft.Linq.Expressions;
using System.IO;

namespace clojure.lang.CljCompiler.Ast
{
    class StaticFieldExpr : FieldExpr
    {
        #region Data

        readonly string _fieldName;
        readonly Type _type;
        readonly FieldInfo _field;
        readonly PropertyInfo _property;

        #endregion

        #region Ctors

        public StaticFieldExpr(Type type, string fieldName)
        {
            _fieldName = fieldName;
            _type = type;
            _field = type.GetField(_fieldName, BindingFlags.Static | BindingFlags.Public);
            _property = type.GetProperty(_fieldName, BindingFlags.Static | BindingFlags.Public);

            if ( _field == null && _property == null  && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
                ((TextWriter)RT.ERR.deref()).WriteLine("Reflection warning {0}:{1} - reference to field/property {2} can't be resolved.", 
                    Compiler.SOURCE_PATH.deref(), /* line */ 0,_fieldName);

        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return true; }
        }

        public override Type ClrType
        {
            get { return _field.FieldType; }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            return Compiler.MaybeBox(GenDlrUnboxed(context));
        }

        public override Expression GenDlrUnboxed(GenContext context)
        {
            return _property != null
                ? Expression.Property(null, _property)
                : Expression.Field(null, _field);
        }

        #endregion

        #region AssignableExpr Members

        public override Expression GenAssignDlr(GenContext context, Expr val)
        {
            Expression access = GenDlrUnboxed(context);
            Expression valExpr = val.GenDlr(context);
            return Expression.Assign(access, valExpr);
        }

        #endregion
    }
}
