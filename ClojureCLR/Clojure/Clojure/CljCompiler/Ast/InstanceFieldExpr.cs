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
using System.IO;
using Microsoft.Linq.Expressions;

namespace clojure.lang.CljCompiler.Ast
{
    class InstanceFieldExpr : FieldExpr
    {
        #region Data

        readonly Expr _target;
        readonly Type _targetType;
        readonly FieldInfo _fieldInfo;
        readonly PropertyInfo _propertyInfo;
        readonly string _fieldName;

        #endregion

        #region Ctors

        public InstanceFieldExpr(Expr target, string fieldName)
        {
            _target = target;
            _fieldName = fieldName;

            _targetType = target.HasClrType ? target.ClrType : null;
            _fieldInfo = _targetType != null ? _targetType.GetField(_fieldName, BindingFlags.Instance | BindingFlags.Public) : null;
            _propertyInfo = _targetType != null ? _targetType.GetProperty(_fieldName, BindingFlags.Instance | BindingFlags.Public) : null;

            if ( _fieldInfo == null && _propertyInfo == null  && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
                ((TextWriter)RT.ERR.deref()).WriteLine("Reflection warning {0}:{1} - reference to field/property {2} can't be resolved.", 
                    Compiler.SOURCE_PATH.deref(), /* line */ 0,_fieldName);
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _fieldInfo != null || _propertyInfo != null; }
        }

        public override Type ClrType
        {
            get {

                return _fieldInfo != null
                    ? _fieldInfo.FieldType
                    : _propertyInfo.PropertyType;
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression target = _target.GenDlr(context);
            if (_targetType != null && (_fieldInfo != null || _propertyInfo != null))
            {
                Expression convTarget = Expression.Convert(target, _targetType);
                Expression access = _fieldInfo != null
                    ? Expression.Field(convTarget, _fieldInfo)
                    : Expression.Property(convTarget, _propertyInfo);
                return Compiler.MaybeBox(access);
            }
            else
                return Compiler.MaybeBox(Expression.PropertyOrField(target,_fieldName));
            //Or maybe this should call Reflector.invokeNoArgInstanceMember
        }

        public override Expression GenDlrUnboxed(GenContext context)
        {
            Expression target = _target.GenDlr(context);
            if (_targetType != null && (_fieldInfo != null || _propertyInfo != null))
            {
                Expression convTarget = Expression.Convert(target, _targetType);
                Expression access = _fieldInfo != null
                    ? Expression.Field(convTarget, _fieldInfo)
                    : Expression.Property(convTarget, _propertyInfo);
                return access;
            }
            else
                throw new InvalidOperationException("Unboxed emit of unknown member.");
        }

        #endregion

        #region AssignableExpr Members

        public override Expression GenAssignDlr(GenContext context, Expr val)
        {
            Expression target = _target.GenDlr(context);
            Expression valExpr = val.GenDlr(context);
            if (_targetType != null)
            {
                Expression convTarget = Expression.Convert(target, _targetType);
                Expression access = _fieldInfo != null
                    ? Expression.Field(convTarget, _fieldInfo)
                    : Expression.Property(convTarget, _propertyInfo);
                return Expression.Assign(access, valExpr);
            }
            else
            {
                // TODO:  Shouldn't this cause a reflection warning?
                Expression call = Expression.Call(
                    target, 
                    Compiler.Method_Reflector_SetInstanceFieldOrProperty,
                    Expression.Constant(_fieldName),
                    valExpr);
                return call;
            }
        }

        #endregion
    }
}
